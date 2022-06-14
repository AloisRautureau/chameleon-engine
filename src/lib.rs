#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use std::time::Instant;
use mimalloc::MiMalloc;

use crate::board::Board;
use crate::move_generator::{GenType, generate};

pub mod bitboard;
pub mod board;
pub mod move_generator;
pub mod piece;
pub mod square;
pub mod uci;
mod r#move;
mod castling;
mod history;
mod evaluation;
mod search;
mod zob_hash;
mod transposition_table;
mod movelist;

pub fn perft(depth: u32, fen: Option<String>) {
    println!("perft");
    let mut board = match fen {
        None => Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
        Some(f) => Board::new(&f)
    };
    println!("{}\n", board);
    println!("depth nodes\n--------");
    for d in 0..depth+1 {
        let start = Instant::now();
        let nodes = _perft(&mut board, d);
        let elapsed = start.elapsed();
        println!("{}     {} ({}s, {} nps)", d, nodes, elapsed.as_secs_f32(), nodes as f32/elapsed.as_secs_f32());
    }
}

fn _perft(board: &mut Board, depth: u32) -> u128 {
    if depth == 0 { return 1 }
    let moves = generate(board, GenType::Legal);
    if depth == 1 { return moves.len() as u128 }
    let mut nodes: u128 = 0u128;
    for mv in &moves {
        board.make(*mv);
        nodes += _perft(board, depth - 1);
        board.unmake()
    }
    nodes
}
