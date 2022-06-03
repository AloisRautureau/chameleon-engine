use crate::piece::{Piece, Color};
use crate::square::{Square, file_of};
use crate::castling::CastlingRights;

// Keys are initialized at compile time for reproducibility and
// to avoid init functions
include!(concat!(env!("OUT_DIR"), "/zobrist_keys.rs"));

pub type Hash = u64;

pub struct ZobristHasher {}
impl ZobristHasher {
    pub fn hash_for_piece_sq(piece: Piece, sq: Square) -> Hash {
        let piece_type_offset = 64 * (piece.piece_type as usize);
        let color_offset = if piece.color == Color::White { 6*64 } else { 0 };
        Self::ZOBRIST_KEYS[piece_type_offset + color_offset + sq]
    }

    pub fn side_to_move_hash(color: Color) -> Hash {
        if color == Color::Black { Self::ZOBRIST_KEYS[Self::BLACK_TO_MOVE_INDEX] } else { 0u64 }
    }

    pub fn castling_rights_hash(castling_rights: CastlingRights) -> Hash {
        let (wking, wqueen) = castling_rights.get(Color::White);
        let (bking, bqueen) = castling_rights.get(Color::Black);
        let mut hash = 0u64;
        if wking { hash ^= Self::ZOBRIST_KEYS[Self::WKING_CASTLE_INDEX] }
        if wqueen { hash ^= Self::ZOBRIST_KEYS[Self::WQUEEN_CASTLE_INDEX] }
        if bking { hash ^= Self::ZOBRIST_KEYS[Self::BKING_CASTLE_INDEX] }
        if bqueen { hash ^= Self::ZOBRIST_KEYS[Self::BQUEEN_CASTLE_INDEX] }
        hash 
    }

    pub fn en_passant_hash(ep_target: Option<Square>) -> Hash {
        if let Some(sq) = ep_target {
            Self::ZOBRIST_KEYS[Self::EP_TARGET_OFFSET + file_of(sq)]
        } else { 0u64 }
    }

    pub const BLACK_TO_MOVE_INDEX: usize = 768;
    pub const WKING_CASTLE_INDEX: usize = 769;
    pub const WQUEEN_CASTLE_INDEX: usize = 770;
    pub const BKING_CASTLE_INDEX: usize = 771;
    pub const BQUEEN_CASTLE_INDEX: usize = 772;
    pub const EP_TARGET_OFFSET: usize = 773;
}