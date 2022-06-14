use std::fmt;
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr, BitAndAssign, BitOrAssign, BitXorAssign};
use crate::square::Square;
use crate::piece::Color;
use crate::piece::PieceType;

// Build script to calculate lookup tables at compile time
include!(concat!(env!("OUT_DIR"), "/lookup.rs"));

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct Bitboard(pub u64);



impl Bitboard {
    pub fn from_square(square: usize) -> Bitboard { Bitboard(1u64 << square) }
    pub fn from_squares(squares: &[usize]) -> Bitboard {
        let mut bits: u64 = 0;
        for sq in squares {
            bits |= 1u64 << sq
        }
        Bitboard(bits)
    }

    pub fn is_set(&self, sq: usize) -> bool { self.0 & (1 << sq) != 0 }
    pub fn set(&mut self, sq: usize) { self.0 |= 1 << sq }
    pub fn unset(&mut self, sq: usize) { self.0 &= !(1 << sq) }

    pub fn ls1b(&self) -> usize {
        if cfg!(target_feature = "bmi1") {
            self.0.trailing_zeros() as usize
        } else {
            // Uses De Bruijn bitscan forward, which might be faster on some
            // CPU's which don't have the BMI set.
            Self::DEBRUIJN_INDEX[(((self.0 & -(self.0 as i64) as u64) * Self::DEBRUIJN_64) >> 58) as usize]
        }
    }

    const DEBRUIJN_64: u64 = 0x03f79d71b4cb0a89;
    const DEBRUIJN_INDEX: [usize; 64] = [
        0,  1, 48,  2, 57, 49, 28,  3,
        61, 58, 50, 42, 38, 29, 17,  4,
        62, 55, 59, 36, 53, 51, 43, 22,
        45, 39, 33, 30, 24, 18, 12,  5,
        63, 47, 56, 27, 60, 41, 37, 16,
        54, 35, 52, 21, 44, 32, 23, 11,
        46, 26, 40, 15, 34, 20, 31, 10,
        25, 14, 19,  9, 13,  8,  7,  6
    ];

    pub fn ms1b(&self) -> usize {
        63 - self.0.leading_zeros() as usize
    }

    pub fn vertical_flip(&self) -> Bitboard { Bitboard(self.0.swap_bytes()) }

    pub fn reset_ls1b(&mut self) { self.0 &= self.0 - 1 }

    pub fn pop_count(&self) -> u32 { self.0.count_ones() }

    /*
    SHIFTS
     */
    pub fn north_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 << 8) }
    pub fn south_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 >> 8) }
    pub fn west_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 >> 1) & !Self::FILES[7] }
    pub fn east_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 << 1) & !Self::FILES[0] }
    pub fn north_west_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 << 7) & !Self::FILES[7] }
    pub fn north_east_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 << 9) & !Self::FILES[0] }
    pub fn south_west_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 >> 9) & !Self::FILES[7] }
    pub fn south_east_shift(bb: Bitboard) -> Bitboard { Bitboard(bb.0 >> 7) & !Self::FILES[0] }

    /*
    GENERALY USEFUL BITBOARDS
    */
    pub const EMPTY: Bitboard = Bitboard(0);
    pub const UNIVERSE: Bitboard = Bitboard(!0);
    pub const FILES: [Bitboard; 8] = [
        Bitboard(0x0101010101010101),
        Bitboard(0x0202020202020202),
        Bitboard(0x0404040404040404),
        Bitboard(0x0808080808080808),
        Bitboard(0x1010101010101010),
        Bitboard(0x2020202020202020),
        Bitboard(0x4040404040404040),
        Bitboard(0x8080808080808080),
    ];
    pub const RANKS: [Bitboard; 8] = [
        Bitboard(0x00000000000000ff),
        Bitboard(0x000000000000ff00),
        Bitboard(0x0000000000ff0000),
        Bitboard(0x00000000ff000000),
        Bitboard(0x000000ff00000000),
        Bitboard(0x0000ff0000000000),
        Bitboard(0x00ff000000000000),
        Bitboard(0xff00000000000000),
    ];
    pub const CASTLING_OCCUPANCY_MASKS: [[Bitboard; 2]; 2] = [
        [Bitboard(0x6000000000000000), Bitboard(0xe00000000000000)], [Bitboard(0x60), Bitboard(0xe)]
    ];
    pub const CASTLING_ATTACKED_MASKS: [[Bitboard; 2]; 2] = [
        [Bitboard(0x6000000000000000), Bitboard(0xc00000000000000)], [Bitboard(0x60), Bitboard(0xc)]
    ];
    pub const CENTER: Bitboard = Bitboard(0x1818000000);
    pub const LARGE_CENTER: Bitboard = Bitboard(0x3c24243c0000);

    pub fn get_ray(origin: Square, target: Square) -> Bitboard {
        Self::ORIGIN_TARGET_RAYS[origin][target]
    }

    pub fn pawn_pushes(pawns_bb: Bitboard, empty: Bitboard, color: Color) -> Bitboard {
        let shift = match color {
            Color::White => Self::north_shift(pawns_bb),
            Color::Black => Self::south_shift(pawns_bb)
        };
        shift & empty
    }

    pub fn pawn_double_pushes(pawns_bb: Bitboard, empty: Bitboard, color: Color) -> Bitboard {
        let single_push = Self::pawn_pushes(pawns_bb, empty, color);
        let shift = match color {
            Color::White => Self::north_shift(single_push & Self::RANKS[2]),
            Color::Black => Self::south_shift(single_push & Self::RANKS[5])
        };
        shift & empty
    }

    pub fn pawn_east_attacks(pawns_bb: Bitboard, color: Color) -> Bitboard {
        match color {
            Color::White => Self::north_east_shift(pawns_bb),
            Color::Black => Self::south_east_shift(pawns_bb)
        }
    }

    pub fn pawn_west_attacks(pawns_bb: Bitboard, color: Color) -> Bitboard {
        match color {
            Color::White => Self::north_west_shift(pawns_bb),
            Color::Black => Self::south_west_shift(pawns_bb)
        }
    }
    pub fn pawn_attacks(pawns_bb: Bitboard, color: Color) -> Bitboard {
        Self::pawn_west_attacks(pawns_bb, color) | Self::pawn_east_attacks(pawns_bb, color)
    }

    pub fn slider_attacks(piece_type: PieceType, origin: Square, occupancy: Bitboard) -> Bitboard {
        match piece_type {
            PieceType::Bishop => Self::bishop_attacks(origin, occupancy),
            PieceType::Rook => Self::rook_attacks(origin, occupancy),
            PieceType::Queen => Self::bishop_attacks(origin, occupancy) | Self::rook_attacks(origin, occupancy),
            _ => Self::EMPTY
        }
    }

    pub fn bishop_attacks(origin: Square, occupancy: Bitboard) -> Bitboard {
        let blockers = occupancy & Self::BISHOP_MASKS[origin];
        let key = (blockers.0 * Self::BISHOP_MAGICS[origin]) >> (64 - Self::BISHOP_MAGIC_SHIFTS[origin]);
        Self::BISHOP_ATTACKS_TABLE[origin][key as usize]
    }
    pub fn xray_bishop_attacks(origin: Square, occupancy: Bitboard, mut blockers: Bitboard) -> Bitboard {
        let attacks = Self::bishop_attacks(origin, occupancy);
        blockers &= attacks;
        attacks ^ Self::bishop_attacks(origin, occupancy ^ blockers)
    }

    pub fn rook_attacks(origin: Square, occupancy: Bitboard) -> Bitboard {
        let blockers = occupancy & Self::ROOK_MASKS[origin];
        let key = (blockers.0 * Self::ROOK_MAGICS[origin]) >> (64 - Self::ROOK_MAGIC_SHIFTS[origin]);
        Self::ROOK_ATTACKS_TABLE[origin][key as usize]
    }
    pub fn xray_rook_attacks(origin: Square, occupancy: Bitboard, mut blockers: Bitboard) -> Bitboard {
        let attacks = Self::rook_attacks(origin, occupancy);
        blockers &= attacks;
        attacks ^ Self::rook_attacks(origin, occupancy ^ blockers)
    }

    pub fn bishop_attacks_setwise(bishops: Bitboard, occupancy: Bitboard) -> Bitboard {
        Self::nw_occlued_fill(bishops, occupancy)
        | Self::sw_occlued_fill(bishops, occupancy)
        | Self::ne_occlued_fill(bishops, occupancy)
        | Self::se_occlued_fill(bishops, occupancy)
    }

    pub fn rook_attacks_setwise(rooks: Bitboard, occupancy: Bitboard) -> Bitboard {
        Self::n_occlued_fill(rooks, occupancy)
        | Self::s_occlued_fill(rooks, occupancy)
        | Self::e_occlued_fill(rooks, occupancy)
        | Self::w_occlued_fill(rooks, occupancy)
    }

    // Kogge-Stone fill algorithms for setwise slider attacks
    fn s_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        origin_set |= blockers & origin_set >> 8;
        blockers &=       blockers >>  8;
        origin_set |= blockers & origin_set >> 16;
        blockers &=       blockers >> 16;
        origin_set |= blockers & origin_set >> 32;
        Self::south_shift(origin_set)
    }

    fn n_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        origin_set |= blockers & origin_set << 8;
        blockers &=       blockers <<  8;
        origin_set |= blockers & origin_set << 16;
        blockers &=       blockers << 16;
        origin_set |= blockers & origin_set << 32;
        Self::north_shift(origin_set)
    }

    fn e_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[0];
        origin_set |= blockers & origin_set << 1;
        blockers &=       blockers << 1;
        origin_set |= blockers & origin_set << 2;
        blockers &=       blockers << 2;
        origin_set |= blockers & origin_set << 4;
        Self::east_shift(origin_set)
    }

    fn ne_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[0];
        origin_set |= blockers & origin_set <<  9;
        blockers &=       blockers <<  9;
        origin_set |= blockers & origin_set << 18;
        blockers &=       blockers << 18;
        origin_set |= blockers & origin_set << 36;
        Self::north_east_shift(origin_set)
    }

    fn se_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[0];
        origin_set |= blockers & origin_set >>  7;
        blockers &=       blockers >>  7;
        origin_set |= blockers & origin_set >> 14;
        blockers &=       blockers >> 14;
        origin_set |= blockers & origin_set >> 28;
        Self::south_east_shift(origin_set)
    }

    fn w_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[7];
        origin_set |= blockers & origin_set >> 1;
        blockers &=       blockers >> 1;
        origin_set |= blockers & origin_set >> 2;
        blockers &=       blockers >> 2;
        origin_set |= blockers & origin_set >> 4;
        Self::west_shift(origin_set)
    }

    fn sw_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[7];
        origin_set |= blockers & origin_set >>  9;
        blockers &=       blockers >>  9;
        origin_set |= blockers & origin_set >> 18;
        blockers &=       blockers >> 18;
        origin_set |= blockers & origin_set >> 36;
        Self::south_west_shift(origin_set)
    }

    fn nw_occlued_fill(mut origin_set: Bitboard, mut blockers: Bitboard) -> Bitboard {
        blockers &= !Self::FILES[7];
        origin_set |= blockers & origin_set <<  7;
        blockers &=       blockers <<  7;
        origin_set |= blockers & origin_set << 14;
        blockers &=       blockers << 14;
        origin_set |= blockers & origin_set << 28;
        Self::north_west_shift(origin_set)
    }
}

impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s: String = String::new();
        let mut line: String = String::new();
        for sq in 0..64 {
            if sq%8 == 0 && sq != 0 {
                line.push('\n');
                s.insert_str(0, &line);
                line = String::new()
            }
            if self.is_set(sq) {
                line.push_str("x ")
            } else {
                line.push_str(". ")
            }
        }
        line.push('\n');
        s.insert_str(0, &line);
        write!(f, "{}", s)
    }
}

// A simple Iterator that resets the LS1B each time
impl Iterator for Bitboard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0u64 { None }
        else {
            let sq = self.ls1b();
            self.reset_ls1b();
            Some(sq)
        }
    }
}

/*
BITWISE OPERATIONS IMPLEMENTATIONS
 */
impl BitAnd for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: Self) -> Self::Output { Bitboard(self.0 & rhs.0) }
}
impl BitAnd<u64> for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: u64) -> Self::Output { Bitboard(self.0 & rhs) }
}
impl BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}
impl BitAndAssign<u64> for Bitboard {
    fn bitand_assign(&mut self, rhs: u64) { self.0 &= rhs }
}
impl BitOr for Bitboard {
    type Output = Bitboard;
    fn bitor(self, rhs: Self) -> Self::Output { Bitboard(self.0 | rhs.0) }
}
impl BitOr<u64> for Bitboard {
    type Output = Bitboard;
    fn bitor(self, rhs: u64) -> Self::Output { Bitboard(self.0 | rhs) }
}
impl BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}
impl BitOrAssign<u64> for Bitboard {
    fn bitor_assign(&mut self, rhs: u64) { self.0 |= rhs }
}
impl BitXor for Bitboard {
    type Output = Bitboard;
    fn bitxor(self, rhs: Self) -> Self::Output { Bitboard(self.0 ^ rhs.0) }
}
impl BitXor<u64> for Bitboard {
    type Output = Bitboard;
    fn bitxor(self, rhs: u64) -> Self::Output { Bitboard(self.0 ^ rhs) }
}
impl BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Bitboard) {
        self.0 ^= rhs.0
    }
}
impl BitXorAssign<u64> for Bitboard {
    fn bitxor_assign(&mut self, rhs: u64) {
        self.0 ^= rhs
    }
}
impl Not for Bitboard {
    type Output = Bitboard;
    fn not(self) -> Self::Output { Bitboard(!self.0) }
}
impl Shl for Bitboard {
    type Output = Bitboard;
    fn shl(self, rhs: Self) -> Self::Output { Bitboard(self.0 << rhs.0) }
}
impl Shl<u64> for Bitboard {
    type Output = Bitboard;
    fn shl(self, rhs: u64) -> Self::Output { Bitboard(self.0 << rhs) }
}
impl Shr for Bitboard {
    type Output = Bitboard;
    fn shr(self, rhs: Self) -> Self::Output { Bitboard(self.0 >> rhs.0) }
}
impl Shr<u64> for Bitboard {
    type Output = Bitboard;
    fn shr(self, rhs: u64) -> Self::Output { Bitboard(self.0 >> rhs) }
}
