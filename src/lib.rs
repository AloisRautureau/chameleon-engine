#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;
use mimalloc::MiMalloc;

pub mod bitboard;
pub mod board;
mod castling;
mod evaluation;
mod history;
mod r#move;
pub mod move_generator;
mod movelist;
pub mod piece;
mod search;
pub mod square;
mod transposition_table;
pub mod uci;
mod zob_hash;
