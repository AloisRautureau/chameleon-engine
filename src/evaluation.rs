use crate::piece::PieceType;
use crate::piece::Color;
use crate::board::Board;
use crate::square::Square;
use crate::bitboard::Bitboard;

use std::cmp::max;

// Build script to calculate evaluation constants at compile time
include!(concat!(env!("OUT_DIR"), "/evaluation_constants.rs"));

pub type Score = i32;

pub const PIECE_TYPE_VALUE: [Score; 6] = [100, 400, 400, 600, 1200, 99999];
pub const MATE_SCORE: Score = i16::MAX as i32;
pub const DRAW_SCORE: Score = 0;
pub const PHASE_VALUE: [i32; 6] = [0, 1, 1, 2, 4, 0];
pub const MAX_PHASE: i32 = 24;

/// Performs a simple, but quick evaluation of a position
pub fn shallow_eval(board: &Board) -> Score {
    if is_drawn(board) { return DRAW_SCORE }

    let mut phase = MAX_PHASE;
    let mut mg_scores = [0, 0];
    let mut eg_scores = [0, 0];

    for color in [Color::Black, Color::White] {
        for (piece, bb) in board.material_iter(color) {
            for sq in if color == Color::White { *bb } else { bb.vertical_flip() } {
                phase -= PHASE_VALUE[piece as usize];
                mg_scores[color as usize] += PIECE_TYPE_VALUE[piece as usize] + MIDGAME_PIECE_SQUARE_TABLE[piece as usize][sq];
                eg_scores[color as usize] += PIECE_TYPE_VALUE[piece as usize] + ENDGAME_PIECE_SQUARE_TABLE[piece as usize][sq];
            }
        }
    }

    phase = (phase * 256 + (MAX_PHASE / 2)) / MAX_PHASE;
    let mg_score = mg_scores[board.side_to_move() as usize] - mg_scores[board.side_to_move().opposite() as usize];
    let eg_score = eg_scores[board.side_to_move() as usize] - eg_scores[board.side_to_move().opposite() as usize];

    ((mg_score * (256 - phase)) + (eg_score * phase)) / 256
}

// TODO: Actually deeper eval
pub fn deep_eval(board: &Board) -> Score {
    if is_drawn(board) { return DRAW_SCORE }
    shallow_eval(board)
}

/// Checks if a position is drawn
fn is_drawn(board: &Board) -> bool {
    if board.is_drawn() { return true }

    let occupancy = board.get_occupancy_bitboard();
    match occupancy.pop_count() {
        2 => true,
        3 => {
            let other = board
                .piece_type_on(
                (board.get_occupancy_bitboard() & !board.get_piecetype_bitboard(PieceType::King)).ls1b()
                ).unwrap();
            other == PieceType::Knight || other == PieceType::Bishop
        },
        4 => {
            let white_others = board.get_color_bitboard(Color::White) & !board.get_piecetype_bitboard(PieceType::King);
            let black_others = board.get_color_bitboard(Color::Black) & !board.get_piecetype_bitboard(PieceType::King);
            white_others.pop_count() == black_others.pop_count()
        },
        _ => false
    }
}

/// Simulates a capture move and returns an approximation of its value
pub fn see(board: &Board, mut origin: Square, target: Square) -> Score {
    // Iterative gains
    let mut gain: [Score; 32] = [0; 32];
    let mut current_depth = 0;

    // Initial capture
    let mut attacking_side = board.side_to_move();
    let mut dealt_with = Bitboard::EMPTY;
    let mut attackers;
    // attacked is None if and only if the capture is en passant, therefore we captured a pawn
    let attacked = board.piece_type_on(target).unwrap_or(PieceType::Pawn); 
    let mut attacker = board.piece_type_on(origin).unwrap();
    gain[0] = PIECE_TYPE_VALUE[attacked as usize];

    // Next captures value
    loop {
        current_depth += 1;
        attacking_side = attacking_side.opposite();

        gain[current_depth] = PIECE_TYPE_VALUE[attacker as usize] - gain[current_depth - 1]; // Score if the square is defended
        if max(-gain[current_depth - 1], gain[current_depth]) < 0 { break; } // We can choose to not recapture

        dealt_with.set(origin);
        attackers = board.attackers_of_square(target, attacking_side, dealt_with) & !dealt_with;

        origin = match least_valuable_attacker_square(board, attackers, attacking_side) {
            Some(sq) => sq,
            None => break
        };
        attacker = board.piece_type_on(origin).unwrap();
    }

    while current_depth != 1{ 
        current_depth -= 1;
        gain[current_depth - 1] = -max(-gain[current_depth - 1], gain[current_depth]);
    }
    gain[0]
}

/// Given a bitboard of attackers, returns the least valuable attacker from that bitboard
fn least_valuable_attacker_square(board: &Board, attackers_bb: Bitboard, attacking_side: Color) -> Option<Square> {
    if attackers_bb != Bitboard::EMPTY {
        for (_, bb) in board.material_iter(attacking_side) {
            let intersection = *bb & attackers_bb;
            if intersection != Bitboard::EMPTY { return Some(intersection.ls1b()) }
        }
    }
    None
}