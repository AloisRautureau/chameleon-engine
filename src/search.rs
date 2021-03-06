use std::fmt::Display;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::board::Board;
use crate::evaluation::{Evaluation, GamePhase, Score};
use crate::move_generator::{generate, GenType};
use crate::movelist::MoveList;
use crate::r#move::Move;
use crate::transposition_table::{HashTable, SearchInfo, TranspositionTable};
use crate::uci::{UCICommand, UCI};
use crate::zob_hash::Hash;

/// A struct to group together every search option available in the UCI
/// protocol.
/// Avoids passing around 6 arguments in functions
#[derive(Clone)]
pub struct SearchOptions {
    pub infinite: bool,
    pub moves_to_search: Option<Vec<Move>>,
    pub moves_until_time_control: Option<u32>,
    pub max_depth: Option<i8>,
    pub max_nodes: Option<u128>,
    pub max_time: Option<Duration>,
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
impl SearchOptions {
    pub fn set_infinite(&mut self, value: bool) -> &mut Self {
        self.infinite = value;
        self
    }
    pub fn set_moves_to_search(&mut self, value: Option<Vec<Move>>) -> &mut Self {
        self.moves_to_search = value;
        self
    }
    pub fn set_moves_until_time_control(&mut self, value: Option<u32>) -> &mut Self {
        self.moves_until_time_control = value;
        self
    }
    pub fn set_depth(&mut self, value: Option<i8>) -> &mut Self {
        self.max_depth = value;
        self
    }
    pub fn set_nodes_to_search(&mut self, value: Option<u128>) -> &mut Self {
        self.max_nodes = value;
        self
    }
    pub fn set_time_from_clock(
        &mut self,
        clock: Duration,
        increment: Option<Duration>,
    ) -> &mut Self {
        self.max_time = Some(Self::get_movetime(clock, increment));
        self
    }
    pub fn set_time(&mut self, value: Option<Duration>) -> &mut Self {
        self.max_time = value;
        self
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

pub struct SearchFramework {
    result: Option<Arc<Mutex<Search>>>,
    transposition_table: Arc<TranspositionTable>,
    searched_moves: Arc<HashTable<[Option<Hash>; 4]>>,
    workers: Vec<Worker>,
}
impl SearchFramework {
    pub fn new() -> SearchFramework {
        SearchFramework {
            result: None,
            transposition_table: Arc::new(TranspositionTable::new()),
            searched_moves: Arc::new(HashTable::new(2_usize.pow(16))),
            workers: vec![],
        }
    }

    pub fn run_search(&mut self, board: &Board, threads: u16, options: &SearchOptions) {
        // A search is currently running
        if self.result.is_some() {
            self.stop_search();
        }
        let result = Arc::new(Mutex::new(Default::default()));

        for _ in 0..threads {
            self.workers.push(new_worker(
                board,
                options,
                &result,
                &self.transposition_table,
                &self.searched_moves,
            ))
        }

        self.result = Some(result);
    }

    pub fn stop_search(&mut self) -> Option<Search> {
        // Stop the workers
        for worker in &self.workers {
            worker.stop_handle.store(true, Ordering::SeqCst);
        }
        self.workers.clear();

        // Return the result if there is one
        let result = if let Some(result_lock) = &self.result {
            let result_ptr = result_lock.lock().unwrap();
            Some(result_ptr.clone())
        } else {
            None
        };

        self.result = None;
        result
    }

    pub fn probe_table(&self, board: &Board) -> SearchInfo {
        let hash = board.get_hash();
        self.transposition_table.get(hash, 0)
    }
}

#[derive(Clone)]
pub struct Search {
    pub finished: bool,
    pub best_move: Option<Move>,
    pub score: Score,
    pub principal_variation: Vec<Move>,
    pub time: Duration,
    pub depth_reached: i8,
    pub nodes_searched: u128,
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
            self.principal_variation
                .iter()
                .fold(String::new(), |acc, m| format!("{} {}", acc, m))
                .trim(),
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
            finished: false,
            best_move: None,
            score: -Evaluation::MATE_SCORE - 1,
            principal_variation: Default::default(),
            time: Duration::ZERO,
            depth_reached: 0,
            nodes_searched: 0,
        }
    }
}

/// Runs a new search with the given options, returning a handle to the thread running said
/// search, as well as an atomic handle to stop the thread
struct Worker {
    pub _handle: JoinHandle<()>,
    pub stop_handle: Arc<AtomicBool>,
}
fn new_worker(
    board: &Board,
    options: &SearchOptions,
    result: &Arc<Mutex<Search>>,
    transposition_table: &Arc<TranspositionTable>,
    searched_moves: &Arc<HashTable<[Option<Hash>; 4]>>,
) -> Worker {
    let stop_handle = Arc::new(AtomicBool::new(false));

    let internal_position = board.clone();
    let internal_options = options.clone();
    let thread_result = Arc::clone(result);
    let stop_signal = Arc::clone(&stop_handle);
    let tt_handle = Arc::clone(transposition_table);
    let searched_handle = Arc::clone(searched_moves);
    let _handle = thread::spawn(move || {
        search_root(
            internal_position,
            internal_options,
            thread_result,
            tt_handle,
            searched_handle,
            stop_signal,
        );
    });

    Worker {
        _handle,
        stop_handle,
    }
}

const MAX_DEPTH: i8 = 32;
const HORIZON: i8 = 0;
const FRONTIER: i8 = 1;
const PRE_FRONTIER: i8 = 2;
const SHALLOW: i8 = 3;
// uses 1/4 of a pawn as the base aspiration window
const BASE_WINDOW: Score = Evaluation::MIDGAME_PIECE_TYPE_VALUE[0] / 4;
/// Searches a given board, writing the results as it goes in a mutex
/// It also sends information in the form of UCI commands during the search
struct SearchContext<'a> {
    pub transposition_table: Arc<TranspositionTable>,
    pub searched_moves: Arc<HashTable<[Option<Hash>; 4]>>,
    pub killers: [[Option<Move>; 3]; 32],
    pub total_depth: i8,
    pub nodes_searched: &'a mut u128,
    pub stop_func: &'a (dyn Fn() -> bool),
    pub should_stop: bool,
    pub sync_func: &'a (dyn Fn(i8) -> bool),
    pub should_sync: bool,
}
impl<'a> SearchContext<'a> {
    pub fn update_flags(&mut self) {
        self.should_stop = (self.stop_func)();
        self.should_sync = (self.sync_func)(self.total_depth);
    }
}
fn search_root(
    mut board: Board,
    options: SearchOptions,
    result: Arc<Mutex<Search>>,
    transposition_table: Arc<TranspositionTable>,
    searched_moves: Arc<HashTable<[Option<Hash>; 4]>>,
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
    let sync_result = Arc::clone(&result);
    let sync_func = move |depth| {
        if let Ok(ptr) = sync_result.try_lock() {
            (*ptr).depth_reached >= depth
        } else {
            false
        }
    };

    let mut context = SearchContext {
        transposition_table,
        searched_moves,
        killers: Default::default(),
        total_depth: 1,
        nodes_searched: &mut nodes,
        stop_func: &stop_func,
        should_stop: false,
        sync_func: &sync_func,
        should_sync: false,
    };

    let root_moves = if let Some(moves) = options.moves_to_search {
        MoveList::from(moves)
    } else {
        generate(&board, GenType::Legal)
    };

    let mut previous_iteration_score = 0;
    let mut previous_iteration_best_move = None;
    'iterative_deepening: while context.total_depth <= options.max_depth.unwrap_or(MAX_DEPTH) {
        let mut iteration_best_move = None;
        let mut iteration_score = -Evaluation::MATE_SCORE - 1;
        let (mut alpha_window, mut beta_window) = (BASE_WINDOW, BASE_WINDOW);
        let (mut alpha, mut beta) = (
            previous_iteration_score - alpha_window,
            previous_iteration_score + beta_window,
        );
        let moves_iter = root_moves.best_first_iter(&score_moves(
            &board,
            context.total_depth,
            previous_iteration_best_move,
            &context,
        ));

        for mv in moves_iter {
            context.update_flags();
            if context.should_stop {
                break 'iterative_deepening;
            }
            if context.should_sync {
                break;
            }
            board.make(*mv);
            let mut score = -alpha_beta(
                &mut board,
                -beta,
                -alpha,
                context.total_depth - 1,
                &mut context,
            );
            // The score falls out of our aspiration window, therefore we widen it
            while (score > -Evaluation::MATE_SCORE && score <= alpha)
                || (score >= beta && score < Evaluation::MATE_SCORE)
            {
                if score <= alpha {
                    alpha_window *= 4
                } else {
                    beta_window *= 4
                };
                alpha = previous_iteration_score - alpha_window;
                beta = previous_iteration_score + beta_window;
                score = -alpha_beta(
                    &mut board,
                    -beta,
                    -alpha,
                    context.total_depth - 1,
                    &mut context,
                );
            }
            if score > iteration_score {
                iteration_best_move = Some(mv);
                iteration_score = score;
                if score > alpha {
                    alpha = score;
                }
            }
            board.unmake();
        }
        if context.should_stop {
            break 'iterative_deepening;
        }

        let mut res = result.lock().unwrap();
        if (*res).finished {
            return;
        }
        if (*res).depth_reached >= context.total_depth {
            // We assume that we're not two whole plies behind
            // Something would be seriously wrong otherwise
            context.total_depth = (*res).depth_reached + 1;
            previous_iteration_score = (*res).score;
            previous_iteration_best_move = (*res).best_move;
            drop(res);
            continue 'iterative_deepening;
        }

        let mv = iteration_best_move.expect("Tried searching a mated board");
        context.transposition_table.set(
            board.get_hash(),
            SearchInfo::Exact {
                position_hash: board.get_hash(),
                best_move: *mv,
                depth_searched: context.total_depth as u8,
                score: iteration_score,
                ply: board.halfmove_clock() as u8,
            },
        );
        (*res).best_move = Some(*mv);
        (*res).score = iteration_score;
        (*res).depth_reached = context.total_depth;
        (*res).time = start.elapsed();
        (*res).principal_variation = collect_pv(&mut board, &context);
        (*res).nodes_searched = *context.nodes_searched;

        UCI::send(UCICommand::Info(&*res));
        drop(res);

        if iteration_score <= -Evaluation::MATE_SCORE || iteration_score >= Evaluation::MATE_SCORE {
            break;
        }

        previous_iteration_score = iteration_score;
        previous_iteration_best_move = Some(*mv);
        context.total_depth += 1;
    }
    let mut res = result.lock().unwrap();
    if !(*res).finished {
        (*res).finished = true;
        UCI::send(UCICommand::BestMove(&(*res).best_move.unwrap()));
    }
}

/// The core alpha beta function
fn alpha_beta(
    board: &mut Board,
    mut alpha: Score,
    beta: Score,
    mut depth: i8,
    context: &mut SearchContext,
) -> Score {
    context.update_flags();

    let in_check = board.in_check(board.side_to_move());
    if in_check {
        depth += 1
    } // Check extension to avoid horizon effect

    if depth <= HORIZON || context.should_stop || context.should_sync {
        return quiescence(board, alpha, beta, context);
    }

    let eval = Evaluation::shallow_eval(board);
    let tt_info = context
        .transposition_table
        .get(board.get_hash(), board.halfmove_clock());

    // Pruning
    let enable_futility_pruning =
        depth <= SHALLOW && !in_check && !board.last_was_capture() && !board.last_was_null();
    if enable_futility_pruning {
        let futility_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[2];
        let extended_futility_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[3];
        let razoring_margin = Evaluation::MIDGAME_PIECE_TYPE_VALUE[4];
        if (depth == FRONTIER && eval.score + futility_margin <= alpha)
            || (depth == PRE_FRONTIER && eval.score + extended_futility_margin <= alpha)
        {
            return quiescence(board, alpha, beta, context);
        } else if depth == SHALLOW && eval.score + razoring_margin <= alpha {
            depth -= 1
        }
    }

    // Null move reduction
    let allow_null = eval.game_phase != GamePhase::EndGame
        && !tt_info.is_pv_node()
        && depth > SHALLOW
        && context.total_depth - depth > 1
        && !board.last_was_null()
        && !board.last_was_capture()
        && !in_check;
    if allow_null {
        let reduced_depth = if depth > 6 { depth - 4 } else { depth - 3 };
        board.make(Move::NULL_MOVE);
        let score = -alpha_beta(board, -beta, -beta + 1, reduced_depth, context);
        board.unmake();

        if score >= beta {
            *context.nodes_searched += 1;
            return score;
        }
    }

    // The table has information about a deeper search that we can use
    // as is
    if depth >= SHALLOW && tt_info.depth_searched() >= Some(depth as u8) {
        *context.nodes_searched += 1;
        return tt_info.bound().unwrap();
    }
    let mut best_move = tt_info.hash_move();

    let moves = generate(board, GenType::Legal);
    if moves.is_empty() {
        return if in_check {
            -Evaluation::MATE_SCORE
        } else {
            Evaluation::DRAW_SCORE
        };
    }
    let moves_iter = moves.best_first_iter(&score_moves(board, depth, best_move, context));
    let mut deferred_moves = vec![];

    let mut pv_search = true;
    let mut best_score = -Evaluation::MATE_SCORE - 1;
    for (mv_index, mv) in moves_iter.enumerate() {
        // We only care about ABDADA on moves
        // other than the first
        if mv_index != 0 && depth >= SHALLOW && notice_search_start(board, mv, context).is_err() {
            // Move is already being searched
            deferred_moves.push((mv_index, *mv));
            continue;
        }
        if !deferred_moves.is_empty() && depth > SHALLOW {
            if let SearchInfo::Cutoff {
                depth_searched,
                lower_bound,
                ..
            } = context
                .transposition_table
                .get(board.get_hash(), board.halfmove_clock())
            {
                // A move searched by another thread has caused a cutoff,
                // so we can save the effort of searching further
                if depth_searched as i8 >= depth {
                    *context.nodes_searched += 1;
                    return lower_bound;
                }
            }
        }

        let score = if pv_search {
            search_pv_move(board, mv, alpha, beta, depth, context)
        } else {
            // Late Move Reduction scheme
            let allow_reduction = mv_index > 4 && !in_check && depth >= SHALLOW && !mv.is_capture();
            search_move(
                board,
                mv,
                mv_index,
                alpha,
                beta,
                depth,
                allow_reduction,
                context,
            )
        };
        if context.should_sync || context.should_stop {
            return best_score;
        }

        if score >= beta {
            context.transposition_table.set(
                board.get_hash(),
                SearchInfo::Cutoff {
                    position_hash: board.get_hash(),
                    refutation_move: *mv,
                    depth_searched: depth as u8,
                    lower_bound: score,
                    ply: board.halfmove_clock() as u8,
                },
            );
            // Store the move for move ordering killer heuristics
            if !mv.is_capture() {
                store_killer(
                    *mv,
                    (context.total_depth - depth) as usize,
                    &mut context.killers,
                )
            }
            *context.nodes_searched += 1;
            return score;
        }
        if score > best_score {
            best_score = score;
            best_move = Some(*mv);
            if score > alpha {
                context.transposition_table.set(
                    board.get_hash(),
                    SearchInfo::Exact {
                        position_hash: board.get_hash(),
                        best_move: *mv,
                        depth_searched: depth as u8,
                        score,
                        ply: board.halfmove_clock() as u8,
                    },
                );
                pv_search = false;
                alpha = score;
            }
        }

        if mv_index != 0 && depth >= SHALLOW {
            notice_search_end(board, mv, context);
        }
    }
    // Now search the moves that we've deferred
    for (mv_index, mv) in deferred_moves {
        let score = if pv_search {
            search_pv_move(board, &mv, alpha, beta, depth, context)
        } else {
            // Late Move Reduction scheme
            let allow_reduction = mv_index > 4 && !in_check && depth >= SHALLOW && !mv.is_capture();
            search_move(
                board,
                &mv,
                mv_index,
                alpha,
                beta,
                depth,
                allow_reduction,
                context,
            )
        };
        if context.should_sync || context.should_stop {
            return best_score;
        }

        if score >= beta {
            context.transposition_table.set(
                board.get_hash(),
                SearchInfo::Cutoff {
                    position_hash: board.get_hash(),
                    refutation_move: mv,
                    depth_searched: depth as u8,
                    lower_bound: score,
                    ply: board.halfmove_clock() as u8,
                },
            );
            // Store the move for killer heuristics
            if !mv.is_capture() {
                store_killer(
                    mv,
                    (context.total_depth - depth) as usize,
                    &mut context.killers,
                )
            }
            *context.nodes_searched += 1;
            return score;
        }
        if score > best_score {
            best_score = score;
            best_move = Some(mv);
            if score > alpha {
                context.transposition_table.set(
                    board.get_hash(),
                    SearchInfo::Exact {
                        position_hash: board.get_hash(),
                        best_move: mv,
                        depth_searched: depth as u8,
                        score,
                        ply: board.halfmove_clock() as u8,
                    },
                );
                pv_search = false;
                alpha = score;
            }
        }
    }

    // We never exceeded alpha
    if pv_search {
        context.transposition_table.set(
            board.get_hash(),
            SearchInfo::All {
                best_move: best_move.unwrap(),
                position_hash: board.get_hash(),
                depth_searched: depth as u8,
                higher_bound: best_score,
                ply: board.halfmove_clock() as u8,
            },
        )
    }
    *context.nodes_searched += 1;
    best_score
}

fn search_pv_move(
    board: &mut Board,
    mv: &Move,
    alpha: Score,
    beta: Score,
    depth: i8,
    context: &mut SearchContext,
) -> Score {
    board.make(*mv);
    let score = -alpha_beta(board, -beta, -alpha, depth - 1, context);
    board.unmake();
    score
}

/// Searches a given move in an alpha-beta framework
fn search_move(
    board: &mut Board,
    mv: &Move,
    mv_index: usize,
    alpha: Score,
    beta: Score,
    depth: i8,
    reduce: bool,
    context: &mut SearchContext,
) -> Score {
    board.make(*mv);
    // Late Move Reduction scheme
    let depth_to_search = if reduce && mv_index < 6 {
        depth - 2
    } else if reduce {
        depth - 1 - (depth / 3)
    } else {
        depth - 1
    };
    let mut score = -alpha_beta(board, -alpha - 1, -alpha, depth_to_search, context);
    // If score is better than alpha, re-search at full depth in case
    // we missed something
    if score > alpha {
        score = -alpha_beta(board, -beta, -alpha, depth - 1, context);
    }
    board.unmake();
    score
}

fn store_killer(mv: Move, ply: usize, killers: &mut [[Option<Move>; 3]; MAX_DEPTH as usize]) {
    for (i, killer) in killers[ply].iter_mut().enumerate() {
        match killer {
            Some(m) => {
                if *m == mv {
                    break;
                } else if i == 2 {
                    *m = mv
                }
            }
            None => {
                *killer = Some(mv);
                break;
            }
        }
    }
    // Swap the ordering around so that we don't always replace
    // the last killer
    killers[ply].swap(0, 1);
    killers[ply].swap(0, 2);
}

/// A special search that tries to find a quiet board in order to reduce the
/// horizon effect
fn quiescence(
    board: &mut Board,
    mut alpha: Score,
    beta: Score,
    context: &mut SearchContext,
) -> Score {
    context.update_flags();

    let legal_moves = generate(board, GenType::Legal);
    if legal_moves.is_empty() {
        *context.nodes_searched += 1;
        return if board.in_check(board.side_to_move()) {
            -Evaluation::MATE_SCORE
        } else {
            Evaluation::DRAW_SCORE
        };
    }

    let mut evaluation = Evaluation::shallow_eval(board);
    evaluation.deep_eval(board);
    if context.should_stop || context.should_sync {
        return evaluation.score;
    }
    if evaluation.is_drawn {
        *context.nodes_searched += 1;
        return evaluation.score;
    }

    if evaluation.score >= beta {
        *context.nodes_searched += 1;
        return evaluation.score;
    }
    if evaluation.score < alpha - Evaluation::MIDGAME_PIECE_TYPE_VALUE[4] {
        *context.nodes_searched += 1;
        return evaluation.score;
    }
    if evaluation.score > alpha {
        alpha = evaluation.score
    }
    let captures = generate(board, GenType::Captures);
    let moves_iter = captures.best_first_iter(&score_quiescence(board));

    let mut best_score = evaluation.score;
    for mv in moves_iter {
        if Evaluation::see(board, *mv) < 0 {
            break;
        }

        board.make(*mv);
        let score = -quiescence(board, -beta, -alpha, context);
        board.unmake();
        if context.should_sync || context.should_stop {
            return best_score;
        }

        if score >= beta {
            *context.nodes_searched += 1;
            return score;
        }
        if score > best_score {
            best_score = score;
            if score > alpha {
                alpha = score;
            }
        }
    }

    *context.nodes_searched += 1;
    best_score
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
    while let Some(m) = context
        .transposition_table
        .get(board.get_hash(), 0)
        .hash_move()
    {
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

// This implements the Simplified ABDADA SMP scheme, as proposed by
// Tom Kerrigan (http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/)
// which is itself a take on Jean-Christophe Weill's ABDADA

// Notices the other threads that we've started searching a move
// if it is not currently searched
fn notice_search_start(board: &Board, mv: &Move, context: &SearchContext) -> Result<(), ()> {
    let move_hash = move_hash(board.get_hash(), mv);
    let lock = context.searched_moves.get(move_hash);
    let mut ptr = lock.lock().unwrap();
    let entry = &mut *ptr;

    // The move is already being searched
    if entry.contains(&Some(move_hash)) {
        return Err(());
    }

    for hash in entry {
        if hash.is_none() {
            *hash = Some(move_hash);
            return Ok(());
        }
    }
    // If we don't find a place to notice the other threads,
    // just put the move hash at the start
    let entry = &mut *ptr;
    entry[0] = Some(move_hash);

    Ok(())
}

// Notices the other threads that we've stopped searching a move
fn notice_search_end(board: &Board, mv: &Move, context: &SearchContext) {
    let move_hash = move_hash(board.get_hash(), mv);
    let lock = context.searched_moves.get(move_hash);
    let mut ptr = lock.lock().unwrap();
    let entry = &mut *ptr;

    for hash in entry {
        match hash {
            Some(h) if *h == move_hash => *hash = None,
            _ => (),
        }
    }
}

fn move_hash(position_hash: Hash, mv: &Move) -> Hash {
    position_hash ^ ((mv.0 as u64 * 1664525) + 1013904223)
}
