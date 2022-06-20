use std::fmt::Display;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::board::Board;
use crate::evaluation::{Evaluation, Score};
use crate::move_generator::{generate, GenType};
use crate::movelist::MoveList;
use crate::r#move::Move;
use crate::transposition_table::{SearchInfo, TranspositionTable};
use crate::uci::{UCICommand, UCI};

/// A struct to group together every search option available in the UCI
/// protocol.
/// Avoids passing around 6 arguments in functions
#[derive(Clone)]
pub struct SearchOptions {
    pub infinite: bool,
    pub moves_to_search: Option<MoveList>,
    pub moves_until_time_control: Option<u32>,
    pub max_depth: Option<i8>,
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
        max_depth: Option<i8>,
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
            max_time: move_time,
        }
    }

    fn get_movetime(clock: Duration, increment: Option<Duration>) -> Duration {
        let mut movetime = clock / 50;
        if let Some(inc) = increment {
            movetime += inc / 2;
            if movetime <= Duration::ZERO {
                return clock;
            }
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
            max_time: None,
        }
    }
}

/// Lets us run the search in a separate thread, so that the UCI implementation
/// stays responsive even during search
pub struct Worker {
    pub handle: JoinHandle<()>,
    pub result: Arc<Mutex<Search>>,
    pub stop_handle: Arc<AtomicBool>
}
pub struct SearchFramework {
    transposition_table: Arc<TranspositionTable>,
    workers: Vec<Worker>
}
impl SearchFramework {
    pub fn new() -> SearchFramework {
        SearchFramework {
            transposition_table: Arc::new(TranspositionTable::new()),
            workers: vec!(),
        }
    }

    pub fn run_search(&mut self, position: &Board, options: &SearchOptions) {
        let thread_count = 1;
        for i in 0..thread_count {
            println!("spawning thread {}", i);
            self.workers.push(Search::new(position, options, &self.transposition_table))
        }
    }

    pub fn stop_search(&mut self) -> Option<Search> {
        let result = if let Some(worker) = self.workers.first() {
            (*worker.result.lock().unwrap()).clone()
        } else {
            return None
        };

        for worker in &self.workers {
            worker.stop_handle.store(true, Ordering::SeqCst);
        }
        self.workers.clear();

        Some(result)
    }

    pub fn probe_table(&self, board: &Board) -> SearchInfo {
        let hash = board.get_hash();
        self.transposition_table.get(hash)
    }
}

#[derive(Clone)]
pub struct Search {
    pub best_move: Option<Move>,
    pub score: Score,
    pub principal_variation: Vec<Move>,
    pub time: Duration,
    pub depth_reached: i8,
    pub nodes_searched: u128,
}
struct SearchContext<'a> {
    pub transposition_table: Arc<TranspositionTable>,
    pub killers: [[Option<Move>; 3]; 32],
    pub total_depth: i8,
    pub null_allowed: bool,
    pub nodes_searched: &'a mut u128,
    pub should_stop: &'a (dyn Fn() -> bool),
}
impl Search {
    const INFINITY: Score = 32767;
    const MAX_DEPTH: i8 = 32;
    const BASE_WINDOW: Score = Evaluation::MIDGAME_PIECE_TYPE_VALUE[0] / 4; // uses 1/4 of a pawn as the aspiration window

    /// Runs a new search with the given options, returning a handle to the thread running said
    /// search, as well as a mutex holding the result
    pub fn new(
        position: &Board,
        options: &SearchOptions,
        transposition_table: &Arc<TranspositionTable>,
    ) -> Worker {
        let result = Arc::new(Mutex::new(Search::default()));
        let stop_handle = Arc::new(AtomicBool::new(false));

        let internal_position = position.clone();
        let internal_options = options.clone();
        let thread_result = Arc::clone(&result);
        let stop_signal = Arc::clone(&stop_handle);
        let tt_handle = Arc::clone(transposition_table);
        let handle = thread::spawn(move || {
            Search::search_root(
                internal_position,
                internal_options,
                thread_result,
                tt_handle,
                true,
                stop_signal,
            );
        });

        Worker {
            handle,
            result,
            stop_handle
        }
    }

    /// Searches a given position, writing the results as it goes in a mutex
    /// It also sends information in the form of UCI commands during the search
    fn search_root(
        mut position: Board,
        options: SearchOptions,
        result: Arc<Mutex<Search>>,
        transposition_table: Arc<TranspositionTable>,
        print_uci: bool,
        stop_signal: Arc<AtomicBool>,
    ) {
        let mut nodes = 0u128;

        let start = Instant::now();
        let stop_func = move || {
            if stop_signal.load(Ordering::SeqCst) {
                true
            } else if !options.infinite {
                if let Some(duration) = options.max_time {
                    if start.elapsed() > duration {
                        return true;
                    }
                }
                if let Some(mn) = options.max_nodes {
                    if nodes > mn {
                        return true;
                    }
                }
                false
            } else {
                false
            }
        };
        let mut context = SearchContext {
            transposition_table,
            killers: Default::default(),
            total_depth: 1,
            null_allowed: true,
            nodes_searched: &mut nodes,
            should_stop: &stop_func,
        };

        let mut root_moves = if let Some(moves) = options.moves_to_search {
            moves
        } else {
            generate(&position, GenType::Legal)
        };

        let (mut alpha, mut beta) = (-Evaluation::MATE_SCORE - 1, Evaluation::MATE_SCORE + 1);

        let mut previous_iteration_score = -Evaluation::MATE_SCORE - 1;
        while !(context.should_stop)()
            && context.total_depth <= options.max_depth.unwrap_or(Self::MAX_DEPTH)
        {
            let mut aspiration_window = (
                previous_iteration_score - Self::BASE_WINDOW,
                previous_iteration_score + Self::BASE_WINDOW,
            );
            let mut iteration_mv_scores: Vec<(Move, Score)> = Vec::with_capacity(root_moves.len());
            let moves_iter = root_moves.into_iter();

            for mv in moves_iter {
                if (context.should_stop)() {
                    break;
                }
                position.make(*mv);
                let mut score = -Search::alpha_beta(
                    &mut position,
                    -beta,
                    -alpha,
                    context.total_depth - 1,
                    &mut context,
                );
                // The score falls out of our aspiration window, therefore we widen it
                if score <= alpha || score >= beta {
                    if score <= alpha {
                        aspiration_window.0 *= 2
                    } else {
                        aspiration_window.1 *= 2
                    };
                    alpha = previous_iteration_score - aspiration_window.0;
                    beta = previous_iteration_score + aspiration_window.1;
                    score = -Search::alpha_beta(
                        &mut position,
                        -beta,
                        -alpha,
                        context.total_depth - 1,
                        &mut context,
                    );
                }
                position.unmake();
                //println!("{} scores {}", mv, score);

                // Insertion  of the move while keeping everything sorted
                // to reuse the ordering in the next iteration
                let insertion_pos =
                    match iteration_mv_scores.binary_search_by(|(_, s)| score.cmp(s)) {
                        Ok(i) => i,
                        Err(i) => i,
                    };
                iteration_mv_scores.insert(insertion_pos, (*mv, score));
            }
            let (mv, score) = *iteration_mv_scores
                .first()
                .expect("Tried searching a mated position");
            
            context.transposition_table.set(
                position.get_hash(),
                SearchInfo::Exact {
                    position_hash: position.get_hash(),
                    best_move: mv,
                    depth_searched: context.total_depth as u8,
                    score,
                },
            );

            let mut res = result.lock().unwrap();
            (*res).best_move = Some(mv);
            (*res).score = score;
            (*res).depth_reached = context.total_depth;
            (*res).time = start.elapsed();
            (*res).principal_variation = Self::collect_pv(&mut position, &context);
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

            if (context.should_stop)() {
                break;
            }
        }

        let res = result.lock().unwrap();
        if print_uci {
            UCI::send(UCICommand::BestMove(
                (*res).best_move.expect("Tried searching a mated position"),
            ))
        }
    }

    /// The core alpha beta function
    fn alpha_beta(
        position: &mut Board,
        mut alpha: Score,
        beta: Score,
        mut depth: i8,
        context: &mut SearchContext,
    ) -> Score {
        *context.nodes_searched += 1;

        if Evaluation::is_drawn(position) {
            return Evaluation::DRAW_SCORE;
        }
        let in_check = position.in_check(position.side_to_move());
        if in_check {
            depth += 1
        } // Check extension to avoid horizon effect

        if depth <= 0 || (context.should_stop)() {
            return Self::quiescence(position, alpha, beta, context);
        }
        // Futility pruning
        let enable_futility_pruning = !in_check && !position.last_was_capture();
        if enable_futility_pruning {
            let futility_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[2];
            let extended_futility_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[3];
            let razoring_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[4];
            if (depth == 1 && Evaluation::shallow_eval(position).score + futility_margin <= alpha)
                || (depth == 2
                    && Evaluation::shallow_eval(position).score + extended_futility_margin <= alpha)
            {
                return Self::quiescence(position, alpha, beta, context);
            } else if depth == 3
                && Evaluation::shallow_eval(position).score + razoring_margin <= alpha
            {
                depth -= 1
            }
        }

        // Null move reduction
        if context.null_allowed && !in_check {
            let reduced_depth = if depth > 6 {
                depth - (4 + (depth as f32).sqrt() as i8)
            } else {
                depth - 3
            };
            context.null_allowed = false;
            position.make(Move::NULL_MOVE);
            let score =
                -Self::alpha_beta(position, -beta, -beta + 1, reduced_depth, context);
            position.unmake();

            if score >= beta {
                return score;
            }
        }
        if !context.null_allowed {
            context.null_allowed = true
        }

        // Check what the eventual TT hit gives us
        let tt_info = context.transposition_table.get(position.get_hash());
        let (mut best_move, mut best_score) = {
            // The table has information about a deeper search that we can use
            // as is
            if tt_info.depth_searched() >= Some(depth as u8) {
                return tt_info.bound().unwrap()
            } else {
                (tt_info.hash_move(), tt_info.bound().unwrap_or(-Evaluation::MATE_SCORE))
            }
        };

        let moves = generate(position, GenType::Legal);
        if moves.is_empty() {
            return if in_check {
                -Evaluation::MATE_SCORE
            } else {
                Evaluation::DRAW_SCORE
            }
        }
        let moves_iter = moves.best_first_iter(&Self::score_moves(position, depth, best_move, context));

        // PV search
        let mut search_pv = true;
        for (mv_index, mv) in moves_iter.enumerate() {
            position.make(*mv);
            let mut score;
            if search_pv {
                score =
                    -Self::alpha_beta(position, -beta, -alpha, depth - 1, context);
            } else {
                // Late Move Reduction scheme
                let allow_reduction = mv_index > 4 && !in_check && depth >= 3 && !mv.is_capture();
                let depth_to_search = if allow_reduction && mv_index < 6 {
                    depth - 2
                } else if allow_reduction {
                    depth >> 1
                } else {
                    depth - 1
                };
                score = -Self::alpha_beta(
                    position,
                    -alpha - 1,
                    -alpha,
                    depth_to_search,
                    context,
                );
                // If score is better than alpha, re-search at full depth in case
                // we missed something
                if score > alpha {
                    score = -Self::alpha_beta(
                        position,
                        -beta,
                        -alpha,
                        depth - 1,
                        context,
                    );
                }
            }
            position.unmake();
            if score >= beta {
                context.transposition_table.set(
                    position.get_hash(),
                    SearchInfo::Cutoff {
                        position_hash: position.get_hash(),
                        refutation_move: *mv,
                        depth_searched: depth as u8,
                        high_bound: beta,
                    },
                );
                // Store the move for move ordering killer heuristics
                if !mv.is_capture() {
                    for (i, killer) in context.killers[(context.total_depth - depth) as usize].iter_mut().enumerate() {
                        match killer {
                            Some(m) => if m == mv { break } else if i == 2 { *m = *mv },
                            None => { *killer = Some(*mv); break }
                        }
                    }
                    // Swap the ordering around so that we don't always replace
                    // the last killer
                    context.killers[(context.total_depth - depth) as usize].swap(0, 1);
                    context.killers[(context.total_depth - depth) as usize].swap(0, 2);
                }
                return beta;
            }
            if score > alpha {
                search_pv = false;
                alpha = score;
            }
            if score > best_score {
                best_score = score;
                best_move = Some(*mv);
            }
        }

        if best_move.is_none() { return alpha }
        if best_score == alpha {
            context.transposition_table.set(
                position.get_hash(),
                SearchInfo::Exact {
                    position_hash: position.get_hash(),
                    best_move: best_move.unwrap(),
                    depth_searched: depth as u8,
                    score: alpha,
                }
            )
        } else {
            context.transposition_table.set(
                position.get_hash(),
                SearchInfo::All {
                    position_hash: position.get_hash(),
                    best_move: best_move.unwrap(),
                    depth_searched: depth as u8,
                    low_bound: alpha,
                }
            )
        }

        alpha
    }

    /// A special search that tries to find a quiet position in order to reduce the
    /// horizon effect
    fn quiescence(
        position: &mut Board,
        mut alpha: Score,
        beta: Score,
        context: &mut SearchContext,
    ) -> Score {
        if generate(position, GenType::Legal).is_empty() {
            return if position.in_check(position.side_to_move()) {
                -Evaluation::MATE_SCORE
            } else {
                Evaluation::DRAW_SCORE
            };
        }

        let mut evaluation = Evaluation::shallow_eval(position);
        if evaluation.is_drawn {
            return evaluation.score;
        }

        if evaluation.score >= beta {
            return beta;
        }
        if evaluation.score < alpha - Evaluation::MIDGAME_PIECE_TYPE_VALUE[4] {
            return alpha;
        }

        evaluation.deep_eval(position);
        if alpha < evaluation.score {
            alpha = evaluation.score
        }
        if (context.should_stop)() {
            return alpha;
        }

        let captures = generate(position, GenType::Captures);
        let moves_iter = captures.best_first_iter(&Self::score_quiescence(position));

        for mv in moves_iter {
            if Evaluation::see(position, *mv) <= 0 {
                break;
            }

            position.make(*mv);
            let score = -Self::quiescence(position, -beta, -alpha, context);
            position.unmake();

            if score >= beta {
                return beta;
            }
            if score > alpha {
                alpha = score
            }
        }

        alpha
    }

    fn score_moves<'a>(
        board: &'a Board,
        depth: i8,
        hash_move: Option<Move>,
        context: &SearchContext,
    ) -> impl Fn(&Move) -> Score + 'a {
        let killers = context.killers[(context.total_depth - depth) as usize];
        move |&m| {
            if Some(m) == hash_move {
                10000
            } else if m.is_capture() {
                Evaluation::see(board, m)
            } else if killers.contains(&Some(m)) {
                200
            } else {
                -10
            }
        }
    }

    fn score_quiescence(board: &Board) -> impl Fn(&Move) -> Score + '_ {
        move |&m| Evaluation::see(board, m)
    }

    fn collect_pv(board: &mut Board, context: &SearchContext) -> Vec<Move> {
        let mut pv = Vec::with_capacity(context.total_depth as usize);
        while let Some(m) = context.transposition_table.get(board.get_hash()).hash_move() {
            board.make(m);
            pv.push(m);
        }
        let mut depth = pv.len();
        while depth != 0 {
            board.unmake();
            depth -= 1;
        }
        pv
    }
}
impl Display for Search {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mate_score =
            self.score <= -Evaluation::MATE_SCORE || self.score >= Evaluation::MATE_SCORE;
        write!(
            f,
            "depth {} time {} nodes {} nps {} pv {} score {} {}",
            self.depth_reached,
            self.time.as_millis(),
            self.nodes_searched,
            ((self.nodes_searched as f64) / self.time.as_secs_f64()) as u64,
            self.principal_variation.iter().fold(String::new(), |acc, m| format!("{} {}", acc, m)).trim(),
            if mate_score { "mate" } else { "cp" },
            if mate_score {
                if self.score < 0 {
                    -(self.principal_variation.len() as Score)
                } else {
                    self.principal_variation.len() as Score
                }
            } else {
                self.score
            }
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
