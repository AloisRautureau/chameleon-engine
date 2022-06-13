use std::fmt::Display;
use std::time::{Duration, Instant};
use std::sync::{Mutex, Arc};
use std::thread::{self, JoinHandle};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::board::Board;
use crate::r#move::Move;
use crate::evaluation::{Score, Evaluation};
use crate::move_generator::{generate, GenType};
use crate::transposition_table::{TranspositionTable, SearchInfo, NodeType};
use crate::uci::{UCI, UCICommand};
use crate::movelist::MoveList;

/// A struct to group together every search option available in the UCI
/// protocol.
/// Avoids passing around 6 arguments in functions
#[derive(Clone)]
pub struct SearchOptions {
    pub infinite: bool,
    pub moves_to_search: Option<MoveList>,
    pub moves_until_time_control: Option<u32>,
    pub max_depth: Option<i32>,
    pub max_nodes: Option<u128>,
    pub max_time: Option<Duration>,
}
impl SearchOptions {
    pub fn from_info(
        infinite: bool,
        moves_to_search: Option<MoveList>,
        clock: Option<Duration>,
        increment: Option<Duration>,
        moves_until_time_control: Option<u32>,
        max_depth: Option<i32>,
        max_nodes: Option<u128>,
        max_time: Option<Duration>,
    ) -> SearchOptions {
        let mut move_time = None;
        if max_time.is_none() {
            if let Some(c) = clock { 
                move_time = Some(Self::get_movetime(c, increment))
            }
        } else {
            move_time = max_time
        }

        SearchOptions {
            infinite,
            moves_to_search,
            moves_until_time_control,
            max_depth,
            max_nodes,
            max_time: move_time
        }
    }

    fn get_movetime(clock: Duration, increment: Option<Duration>) -> Duration {
        let mut movetime = clock/50;
        if let Some(inc) = increment {
            movetime += inc/2;
            if movetime <= Duration::ZERO { return clock }
        }

        movetime
    }
}
impl Default for SearchOptions {
    fn default() -> Self {
        SearchOptions {
            infinite: true,
            moves_to_search: None,
            moves_until_time_control: None,
            max_depth: None,
            max_nodes: None,
            max_time: None
        }
    }
}

/// Lets us run the search in a separate thread, so that the UCI implementation
/// stays responsive even during search
pub struct SearchFramework {
    transposition_table: Arc<TranspositionTable>,
    search_handle: Option<JoinHandle<()>>,
    search_result: Option<Arc<Mutex<Search>>>,
    stop_handle: Option<Arc<AtomicBool>>
}
impl SearchFramework {
    pub fn new() -> SearchFramework {
        SearchFramework {
            transposition_table: Arc::new(TranspositionTable::new(16384 * 2)),
            search_handle: None,
            search_result: None,
            stop_handle: None,
        }
    }

    pub fn run_search(&mut self, position: &Board, options: SearchOptions) {
        let (search_handle, stop_handle, search_result) = Search::new(position, options, &self.transposition_table);
        self.search_handle = Some(search_handle);
        self.search_result = Some(search_result);
        self.stop_handle = Some(stop_handle);
    }

    pub fn stop_search(&mut self) -> Option<Search> {
        self.stop_handle.as_ref().unwrap().store(true, Ordering::SeqCst);
        self.search_handle = None;
        match &self.search_result {
            Some(arc_mutex) => {
                let result = (*arc_mutex.lock().unwrap()).clone();
                self.search_result = None;
                Some(result)
            }
            None => None
        }
    }

    pub fn probe_table(&self, board: &Board) -> Option<SearchInfo> {
        let hash = board.get_hash();
        self.transposition_table.get(hash)
    }
}


#[derive(Clone)]
pub struct Search {
    pub best_move: Option<Move>,
    pub score: Score,
    pub principal_variation: MoveList,
    pub time: Duration,
    pub depth_reached: i32,
    pub nodes_searched: u128,
}
struct SearchContext<'a> {
    pub transposition_table: Arc<TranspositionTable>,
    pub killers: [Option<Move>; 32],
    pub pv: MoveList,
    pub total_depth: i32,
    pub null_allowed: bool,
    pub nodes_searched: &'a mut u128,
    pub should_stop: &'a (dyn Fn() -> bool)
}
impl Search {
    const INFINITY: i32 = i32::MAX;
    const MAX_DEPTH: i32 = 32;
    const BASE_WINDOW: i32 = Evaluation::MIDGAME_PIECE_TYPE_VALUE[0] / 4; // uses 1/4 of a pawn as the aspiration window

    /// Runs a new search with the given options, returning a handle to the thread running said
    /// search, as well as a mutex holding the result
    /// TODO: This might be more properly done by using async programming patterns
    pub fn new(position: &Board, options: SearchOptions, transposition_table: &Arc<TranspositionTable>) -> (JoinHandle<()>, Arc<AtomicBool>, Arc<Mutex<Search>>) {
        let result = Arc::new(Mutex::new(Search::default()));
        let stop_handle = Arc::new(AtomicBool::new(false));

        // Main search worker
        let internal_position = position.clone();
        let internal_options = options.clone();
        let thread_result = Arc::clone(&result);
        let stop_signal = Arc::clone(&stop_handle);
        let tt_handle = Arc::clone(&transposition_table);
        let handle = thread::spawn(move || {
            Search::search_root(internal_position, internal_options, thread_result, tt_handle, true, stop_signal);
        });

        (handle, stop_handle, result)
    }

    /// Searches a given position, writing the results as it goes in a mutex
    /// It also sends information in the form of UCI commands during the search
    fn search_root(mut position: Board, options: SearchOptions, result: Arc<Mutex<Search>>, transposition_table: Arc<TranspositionTable>, print_uci: bool, stop_signal: Arc<AtomicBool>) {
        let mut nodes = 0u128;

        let start = Instant::now();
        let stop_func = move || {
            if stop_signal.load(Ordering::SeqCst) { true }
            else if !options.infinite {
                if let Some(duration) = options.max_time {
                    if start.elapsed() > duration { return true }
                }
                if let Some(mn) = options.max_nodes {
                    if nodes > mn { return true }
                }
                false
            } else {
                false
            }
        };
        let mut context = SearchContext {
            transposition_table,
            killers: Default::default(),
            pv: Default::default(),
            total_depth: 1,
            null_allowed: true,
            nodes_searched: &mut nodes,
            should_stop: &stop_func
        };

        let mut root_moves = if let Some(moves) = options.moves_to_search {
            moves
        } else {
            generate(&position, GenType::Legal)
        };

        let (mut alpha, mut beta) = (-Evaluation::MATE_SCORE-1, Evaluation::MATE_SCORE+1);

        let mut previous_iteration_score = -Evaluation::MATE_SCORE-1;
        while !(context.should_stop)() && context.total_depth <= options.max_depth.unwrap_or(Self::MAX_DEPTH) {
            let mut aspiration_window = (previous_iteration_score - Self::BASE_WINDOW, previous_iteration_score + Self::BASE_WINDOW);
            let mut iteration_pv = VecDeque::with_capacity(context.total_depth as usize);
            let mut iteration_mv_scores: Vec<(Move, Score)> = Vec::with_capacity(root_moves.len());
            let moves_iter = root_moves.into_iter();

            for mv in moves_iter {
                if (context.should_stop)() { break }
                let mut move_pv = VecDeque::with_capacity(context.total_depth as usize);
                position.make(mv);
                let mut score = -Search::alpha_beta(&mut position, -beta, -alpha, context.total_depth - 1, &mut move_pv, &mut context);
                // The score falls out of our aspiration window, therefore we widen it
                if score <= alpha || score >= beta {
                    if score <= alpha { aspiration_window.0 *= 2 } else { aspiration_window.1 *= 2 };
                    alpha = previous_iteration_score - aspiration_window.0;
                    beta = previous_iteration_score + aspiration_window.1;
                    score = -Search::alpha_beta(&mut position, -beta, -alpha, context.total_depth - 1, &mut move_pv, &mut context);
                }
                position.unmake();

                // Insertion  of the move while keeping everything sorted
                // to reuse the ordering in the next iteration
                let insertion_pos = match iteration_mv_scores.binary_search_by(|(_, s)| score.cmp(s)) {
                    Ok(i) => i,
                    Err(i) => i
                };
                iteration_mv_scores.insert(insertion_pos, (mv, score));
                if insertion_pos == 0 {
                    iteration_pv = move_pv;
                    iteration_pv.push_front(mv);
                }
            }
            context.pv = MoveList::from(iteration_pv);

            let (mv, score) = *iteration_mv_scores.first().expect("Tried searching a mated position");

            context.transposition_table.set(position.get_hash(), SearchInfo {
                position_hash: position.get_hash(),
                best_move: Some(mv),
                depth_searched: context.total_depth,
                score,
                node_type: NodeType::Exact,
                age: position.halfmove_clock() % 2 == 0
            });

            let mut res = result.lock().unwrap();
            (*res).best_move = Some(mv);
            (*res).score = score;
            (*res).depth_reached = context.total_depth;
            (*res).time = start.elapsed();
            (*res).principal_variation = context.pv;
            (*res).nodes_searched = *context.nodes_searched;

            if print_uci {
                UCI::send(UCICommand::Info(&*res));
            }
            drop(res);

            previous_iteration_score = score;
            // Sort the moves depending on their current score
            root_moves = MoveList::default();
            for (mv, _) in iteration_mv_scores {
                root_moves.push(mv);
            }
            context.total_depth += 1;

            if (context.should_stop)() { break }
        }

        let res = result.lock().unwrap();
        if print_uci {
            UCI::send(UCICommand::BestMove((*res).best_move.expect("Tried searching a mated position")))
        }
    }

    /// The core alpha beta function
    fn alpha_beta(position: &mut Board, mut alpha: Score, beta: Score, mut depth: i32, local_pv: &mut VecDeque<Move>, context: &mut SearchContext) -> Score {
        *context.nodes_searched += 1;

        if Evaluation::is_drawn(position) { return Evaluation::DRAW_SCORE }
        let in_check = position.in_check(position.side_to_move());
        if in_check { depth += 1 } // Check extension to avoid horizon effect

        if depth <= 0 || (context.should_stop)() {
            return Self::quiescence(position, alpha, beta, context)
        }
        // Futility pruning
        let enable_futility_pruning = !in_check && !position.last_was_capture();
        if depth == 1 && enable_futility_pruning {
            if Evaluation::shallow_eval(position).score + Evaluation::MIDGAME_PIECE_TYPE_VALUE[2] <= alpha {
                return Self::quiescence(position, alpha, beta, context)
            }
        }
        if depth == 2 && enable_futility_pruning { // Extended
            if Evaluation::shallow_eval(position).score + Evaluation::MIDGAME_PIECE_TYPE_VALUE[3] <= alpha {
                return Self::quiescence(position, alpha, beta, context)
            }
        }
        if depth == 3 && enable_futility_pruning { // Razoring
            if Evaluation::shallow_eval(position).score + Evaluation::MIDGAME_PIECE_TYPE_VALUE[4] <= alpha {
                depth -= 1
            }
        }

        // Null move reduction
        if context.null_allowed && !in_check {
            let reduced_depth = if depth > 6 {
                depth - (4 + (depth as f32).sqrt() as i32)
            } else {
                depth - 3
            };
            context.null_allowed = false;
            position.make(Move::NULL_MOVE);
            let score = -Self::alpha_beta(position, -beta, -beta+1, reduced_depth, local_pv, context);
            position.unmake();

            if score >= beta { return score }
        }
        if !context.null_allowed { context.null_allowed = true }

        let moves = generate(position, GenType::Legal);
        if moves.is_empty() { 
            return if in_check { -Evaluation::MATE_SCORE } else { Evaluation::DRAW_SCORE }
        }
        let moves_iter = moves.best_first_iter(&Self::score_moves(&position, depth, &context));

        // PV search
        let mut search_pv = true;

        for (mv_index, mv) in moves_iter.enumerate() {
            let mut move_pv = VecDeque::with_capacity(depth as usize);
            position.make(mv);
            let mut score;
            if search_pv {
                score = -Self::alpha_beta(position, -beta, -alpha, depth - 1, &mut move_pv, context);
            } else {
                // Late Move Reduction scheme
                let allow_reduction = mv_index > 4 && !in_check && depth <= 3 && !mv.is_capture();
                let depth_to_search = if allow_reduction && mv_index < 6 {
                    depth - 2
                } else if allow_reduction {
                    depth >> 2
                } else {
                    depth - 1
                };
                score = -Self::alpha_beta(position, -alpha-1, -alpha, depth_to_search, &mut move_pv, context);
                // If score is better than alpha, re-search at full depth in case
                // we missed something
                if score > alpha {
                    score = -Self::alpha_beta(position, -beta, -alpha, depth - 1, &mut move_pv, context);
                }
            }
            position.unmake();
            if score >= beta {
                context.transposition_table.set(position.get_hash(), SearchInfo {
                    position_hash: position.get_hash(),
                    best_move: Some(mv),
                    depth_searched: depth,
                    score,
                    node_type: NodeType::Beta,
                    age: position.halfmove_clock() % 2 == 0
                });
                if context.killers[depth as usize].is_none() {
                    context.killers[depth as usize] = Some(mv) 
                }
                return beta;
            } 
            if score > alpha {
                context.transposition_table.set(position.get_hash(), SearchInfo {
                    position_hash: position.get_hash(),
                    best_move: Some(mv),
                    depth_searched: depth,
                    score,
                    node_type: NodeType::Exact,
                    age: position.halfmove_clock() % 2 == 0
                });
                *local_pv = move_pv;
                local_pv.push_front(mv);
                search_pv = false;
                alpha = score
            }
        }

        alpha
    }

    /// A special search that tries to find a quiet position in order to reduce the
    /// horizon effect
    fn quiescence(position: &mut Board, mut alpha: Score, beta: Score, context: &mut SearchContext) -> Score {
        if generate(position, GenType::Legal).is_empty() {
            return if position.in_check(position.side_to_move()) { -Evaluation::MATE_SCORE } else { Evaluation::DRAW_SCORE }
        }

        let mut evaluation = Evaluation::shallow_eval(position);
        if evaluation.is_drawn { return evaluation.score; }

        if evaluation.score >= beta { return beta }
        if evaluation.score < alpha - Evaluation::MIDGAME_PIECE_TYPE_VALUE[4] { return alpha }

        evaluation.deep_eval(position);
        if alpha < evaluation.score { alpha = evaluation.score }
        if (context.should_stop)() { return alpha }

        let captures = generate(position, GenType::Captures);
        let moves_iter = captures.best_first_iter(&Self::score_quiescence(&position));

        for mv in moves_iter {
            if Evaluation::see(position, mv) <= 0 { break; }

            position.make(mv);
            let score = -Self::quiescence(position, -beta, -alpha, context);
            position.unmake();

            if score >= beta { return beta }
            if score > alpha { alpha = score }
        }

        alpha
    }

    fn score_moves<'a>(board: &'a Board, depth: i32, context: &SearchContext) -> impl Fn(Move) -> Score + 'a {
        let pv_move = context.pv.get((context.total_depth - depth) as usize);
        let killer = context.killers[(context.total_depth - depth) as usize];
        let hash_move = context.transposition_table
            .get(board.get_hash())
            .map(|i| i.best_move.unwrap());
        move |m| {
            if Some(m) == pv_move { 1000000 }
            else if Some(m) == hash_move { 100000 }
            else if Some(m) == killer { 10000 }
            else { Evaluation::see(&board, m) }
        }
    }

    fn score_quiescence<'a>(board: &'a Board) -> impl Fn(Move) -> Score + 'a {
        move |m| {
            Evaluation::see(&board, m)
        }
    }
}
impl Display for Search {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mate_score = self.score <= -Evaluation::MATE_SCORE || self.score >= Evaluation::MATE_SCORE;
        write!(
            f, "depth {} time {} nodes {} nps {} pv {} score {} {}",
            self.depth_reached, self.time.as_millis(),
            self.nodes_searched, ((self.nodes_searched as f64)/self.time.as_secs_f64()) as u64, 
            self.principal_variation, 
            if mate_score { "mate" } else { "cp" },
            if mate_score {
                if self.score < 0 {
                    -(self.principal_variation.len() as i32)
                } else {
                    self.principal_variation.len() as i32
                }
            } else { self.score }
        )
    }
}
impl Default for Search {
    fn default() -> Self {
        Search {
            best_move: None,
            score: -Self::INFINITY,
            principal_variation: Default::default(),
            time: Duration::ZERO,
            depth_reached: 0,
            nodes_searched: 0,
        }
    }
}
