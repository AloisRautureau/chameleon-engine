use crate::piece::Color::Black;
use crate::piece::PieceType::{Bishop, King, Knight, Pawn, Queen, Rook};
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub color: Color,
}
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}
impl PieceType {
    pub fn from_determinant(i: usize) -> Option<PieceType> {
        match i {
            0 => Some(Pawn),
            1 => Some(Knight),
            2 => Some(Bishop),
            3 => Some(Rook),
            4 => Some(Queen),
            5 => Some(King),
            _ => None,
        }
    }
    pub fn can_slide(&self) -> bool {
        matches!(self, Bishop | Queen | Rook)
    }
    pub fn is_bishop_like(&self) -> bool {
        matches!(self, Bishop | Queen)
    }
    pub fn is_rook_like(&self) -> bool {
        matches!(self, Queen | Rook)
    }
}
impl Display for PieceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pawn => "p",
                Knight => "n",
                Bishop => "b",
                Rook => "r",
                Queen => "q",
                King => "k",
            }
        )
    }
}
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
pub enum Color {
    Black,
    White,
}
impl Color {
    pub fn opposite(&self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}
impl Display for Color {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self == &Black { "b" } else { "w" })
    }
}

impl Piece {
    pub fn from_char(c: char) -> Option<Piece> {
        let piece_type = match c.to_lowercase().next().unwrap_or('_') {
            'p' => PieceType::Pawn,
            'n' => PieceType::Knight,
            'b' => PieceType::Bishop,
            'r' => PieceType::Rook,
            'q' => PieceType::Queen,
            'k' => PieceType::King,
            _ => return None,
        };
        let color = if c.is_lowercase() {
            Color::Black
        } else {
            Color::White
        };
        Some(Piece { piece_type, color })
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s: String = String::from(match self.piece_type {
            PieceType::Pawn => "p",
            PieceType::Knight => "n",
            PieceType::Bishop => "b",
            PieceType::Rook => "r",
            PieceType::Queen => "q",
            PieceType::King => "k",
        });
        write!(
            f,
            "{}",
            if self.color == Color::White {
                s.to_uppercase()
            } else {
                s
            }
        )
    }
}
