use crate::castling::CastlingRights;
use crate::piece::Piece;
use crate::r#move::Move;
use crate::square::Square;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct HistoryEntry {
    pub move_played: Move,
    pub captured_piece: Option<Piece>,
    pub ep_target: Option<Square>,
    pub castling_rights: CastlingRights,
    pub reversible_moves: u32,
}
