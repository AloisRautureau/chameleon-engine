use crate::piece::PieceType::{Bishop, Knight, Queen, Rook};
use crate::piece::{Color, PieceType};
use crate::square::{self, square_representation, Square};
use std::fmt::{Display, Formatter};

/// Moves are stored as a 2bytes word, with the following alignment:
/// - 6*2 bits for origin and destination square
/// - 4 bits used for various flags
#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Move(u16);

impl Move {
    fn new(origin: u16, target: u16, flags: u16) -> Move {
        Move((origin << 10) | (target << 4) | flags)
    }
    pub fn new_quiet(origin: Square, target: Square) -> Move {
        Self::new(origin as u16, target as u16, 0b0)
    }
    pub fn new_double_push(origin: Square, target: Square) -> Move {
        Self::new(origin as u16, target as u16, 0b1)
    }
    pub fn new_capture(origin: Square, target: Square) -> Move {
        Self::new(origin as u16, target as u16, 0b100)
    }
    pub fn new_en_passant(origin: Square, target: Square) -> Move {
        Self::new(origin as u16, target as u16, 0b101)
    }
    pub fn new_kingside_castle(color: Color) -> Move {
        match color {
            Color::White => Move(4194),
            Color::Black => Move(62434),
        }
    }
    pub fn new_queenside_castle(color: Color) -> Move {
        match color {
            Color::White => Move(4131),
            Color::Black => Move(62371),
        }
    }
    pub fn new_promotion(origin: Square, target: Square, promote_to: PieceType) -> Move {
        Self::new(
            origin as u16,
            target as u16,
            0b1000
                | match promote_to {
                    PieceType::Knight => 0b00,
                    PieceType::Bishop => 0b01,
                    PieceType::Rook => 0b10,
                    _ => 0b11,
                },
        )
    }
    pub fn all_promotions(origin: Square, target: Square) -> [Move; 4] {
        [
            Self::new_promotion(origin, target, Knight),
            Self::new_promotion(origin, target, Bishop),
            Self::new_promotion(origin, target, Rook),
            Self::new_promotion(origin, target, Queen),
        ]
    }
    pub fn new_promotion_capture(origin: Square, target: Square, promote_to: PieceType) -> Move {
        Self::new(
            origin as u16,
            target as u16,
            0b1100
                | match promote_to {
                    PieceType::Knight => 0b00,
                    PieceType::Bishop => 0b01,
                    PieceType::Rook => 0b10,
                    _ => 0b11,
                },
        )
    }
    pub fn all_promotion_captures(origin: Square, target: Square) -> [Move; 4] {
        [
            Self::new_promotion_capture(origin, target, Knight),
            Self::new_promotion_capture(origin, target, Bishop),
            Self::new_promotion_capture(origin, target, Rook),
            Self::new_promotion_capture(origin, target, Queen),
        ]
    }
    pub const NULL_MOVE: Move = Move(0xff);

    pub fn origin(&self) -> Square {
        (self.0 >> 10) as Square
    }
    pub fn target(&self) -> Square {
        ((self.0 >> 4) & 0b111111) as Square
    }
    pub fn flags(&self) -> MoveFlags {
        match self.0 & 0b1111 {
            0b0001 => MoveFlags::DoublePush,
            0b0010 => MoveFlags::KingSideCastle,
            0b0011 => MoveFlags::QueenSideCastle,
            0b0100 => MoveFlags::Capture,
            0b0101 => MoveFlags::EnPassant,
            0b1000 => MoveFlags::Promotion(PieceType::Knight),
            0b1001 => MoveFlags::Promotion(PieceType::Bishop),
            0b1010 => MoveFlags::Promotion(PieceType::Rook),
            0b1011 => MoveFlags::Promotion(PieceType::Queen),
            0b1100 => MoveFlags::PromotionCapture(PieceType::Knight),
            0b1101 => MoveFlags::PromotionCapture(PieceType::Bishop),
            0b1110 => MoveFlags::PromotionCapture(PieceType::Rook),
            0b1111 => MoveFlags::PromotionCapture(PieceType::Queen),
            _ => MoveFlags::Quiet,
        }
    }

    pub fn is_capture(&self) -> bool {
        self.0 & 0b100 != 0 && *self != Self::NULL_MOVE
    }

    pub fn promotion_target(&self) -> Option<PieceType> {
        match self.flags() {
            MoveFlags::Promotion(p) | MoveFlags::PromotionCapture(p) => Some(p),
            _ => None,
        }
    }

    /// Parses a move formatted in long algebraic notation.
    /// Since no information can be given on flags, it simply returns origin, target and potential
    /// piece type to promote to
    pub fn parse(mv: &str) -> Option<(Square, Square, Option<PieceType>)> {
        let move_string = String::from(mv);
        let origin = square::parse_square(&move_string[0..2])?;
        let target = square::parse_square(&move_string[2..4])?;
        let promotion_target = if move_string.len() == 5 {
            match &move_string[4..] {
                "b" => Some(PieceType::Bishop),
                "n" => Some(PieceType::Knight),
                "r" => Some(PieceType::Rook),
                "q" => Some(PieceType::Queen),
                _ => None,
            }
        } else {
            None
        };
        Some((origin, target, promotion_target))
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let o = square_representation(self.origin()).unwrap_or_else(|| String::from("**"));
        let t = square_representation(self.target()).unwrap_or_else(|| String::from("**"));
        let promotion = self.promotion_target();
        if let Some(p) = promotion {
            write!(f, "{}{}{}", o, t, p)
        } else {
            write!(f, "{}{}", o, t)
        }
    }
}

impl Default for Move {
    fn default() -> Self {
        Self::NULL_MOVE
    }
}

pub enum MoveFlags {
    Quiet,
    DoublePush,
    KingSideCastle,
    QueenSideCastle,
    Capture,
    EnPassant,
    Promotion(PieceType),
    PromotionCapture(PieceType),
}
