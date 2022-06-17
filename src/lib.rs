#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;
use mimalloc::MiMalloc;

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
