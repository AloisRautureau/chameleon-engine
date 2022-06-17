use crate::piece::Color;
use crate::piece::Color::{Black, White};
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct CastlingRights(u8);

impl CastlingRights {
    pub fn new() -> CastlingRights {
        CastlingRights(0b1111)
    }
    pub fn from_str(s: &str) -> CastlingRights {
        let mut res = CastlingRights(0);
        if s.contains('K') {
            res.0 |= 0b1000
        }
        if s.contains('Q') {
            res.0 |= 0b0100
        }
        if s.contains('k') {
            res.0 |= 0b0010
        }
        if s.contains('q') {
            res.0 |= 0b0001
        }
        res
    }

    /// Returns the castling rights of a given color
    pub fn get(&self, side: Color) -> (bool, bool) {
        match side {
            White => (self.0 & 0b1000 != 0, self.0 & 0b0100 != 0),
            _ => (self.0 & 0b10 != 0, self.0 & 0b01 != 0),
        }
    }
    /// Marks the given side as unable to castle
    pub fn uncastle(&mut self, side: Color) {
        self.0 &= match side {
            White => 0b0011,
            _ => 0b1100,
        }
    }
    /// Marks the given side as unable to castle kingside
    pub fn uncastle_kingside(&mut self, side: Color) {
        self.0 &= match side {
            White => 0b0111,
            _ => 0b1101,
        }
    }
    /// Marks the given side as unable to castle queenside
    pub fn uncastle_queenside(&mut self, side: Color) {
        self.0 &= match side {
            White => 0b1011,
            _ => 0b1110,
        }
    }
}

impl Display for CastlingRights {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (wking, wqueen) = self.get(White);
        let (bking, bqueen) = self.get(Black);
        write!(
            f,
            "{}{}{}{}",
            if wking { "K" } else { "" },
            if wqueen { "Q" } else { "" },
            if bking { "k" } else { "" },
            if bqueen { "q" } else { "" }
        )
    }
}

impl Default for CastlingRights {
    fn default() -> Self {
        Self::new()
    }
}
