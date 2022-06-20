use crate::castling::CastlingRights;
use crate::piece::Piece;
use crate::r#move::Move;
use crate::square::Square;
use crate::zob_hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct HistoryEntry {
    pub hash: Hash,
    pub move_played: Move,
    pub captured_piece: Option<Piece>,
    pub ep_target: Option<Square>,
    pub castling_rights: CastlingRights,
    pub reversible_moves: u32,
    pub last_irreversible_ply: usize,
}
