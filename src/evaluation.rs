use crate::piece::PieceType;
use crate::piece::Color;
use crate::board::Board;
use crate::square::Square;
use crate::bitboard::Bitboard;

use std::cmp::max;
use crate::r#move::Move;

// Build script to calculate evaluation constants at compile time
include!(concat!(env!("OUT_DIR"), "/evaluation_constants.rs"));

pub type Score = i32;
pub enum GamePhase {
    Opening,
    MiddleGame,
    EndGame
}

pub struct Evaluation {
    pub score: Score,
    pub game_phase: GamePhase,
    pub is_drawn: bool,
    pub full_eval: bool,
}
impl Evaluation {
    pub const MIDGAME_PIECE_TYPE_VALUE: [Score; 6] = [100, 300, 350, 500, 900, 99999];
    pub const ENDGAME_PIECE_TYPE_VALUE: [Score; 6] = [120, 250, 300, 550, 850, 99999];
    pub const MATE_SCORE: Score = i16::MAX as i32;
    pub const DRAW_SCORE: Score = 0;
    pub const PHASE_VALUE: [i32; 6] = [0, 1, 1, 2, 4, 0];
    pub const MAX_PHASE: i32 = 24;

    /// Performs a simple, but quick evaluation of a position
    pub fn shallow_eval(board: &Board) -> Evaluation {
        if Self::is_drawn(board) {
            return Evaluation {
                score: Self::DRAW_SCORE,
                game_phase: GamePhase::EndGame,
                is_drawn: true,
                full_eval: true
            }
        }

        let mut phase = Self::MAX_PHASE;
        let mut mg_scores = [0, 0];
        let mut eg_scores = [0, 0];

        for color in [Color::Black, Color::White] {
            for (piece, bb) in board.material_iter(color).enumerate() {
                for sq in if color == Color::White { *bb } else { bb.vertical_flip() } {
                    phase -= Self::PHASE_VALUE[piece];
                    mg_scores[color as usize] += Self::MIDGAME_PIECE_TYPE_VALUE[piece] + MIDGAME_PIECE_SQUARE_TABLE[piece][sq];
                    eg_scores[color as usize] += Self::ENDGAME_PIECE_TYPE_VALUE[piece] + ENDGAME_PIECE_SQUARE_TABLE[piece][sq];
                }
            }
        }

        let game_phase = if phase >= 20 {
            GamePhase::Opening
        } else if phase >= 10 {
            GamePhase::MiddleGame
        } else {
            GamePhase::EndGame
        };
        phase = (phase * 256 + (Self::MAX_PHASE / 2)) / Self::MAX_PHASE;
        let mg_score = mg_scores[board.side_to_move() as usize] - mg_scores[board.side_to_move().opposite() as usize];
        let eg_score = eg_scores[board.side_to_move() as usize] - eg_scores[board.side_to_move().opposite() as usize];

        Evaluation {
            score: ((mg_score * (256 - phase)) + (eg_score * phase)) / 256,
            game_phase,
            is_drawn: false,
            full_eval: false
        }

    }

    pub fn deep_eval(&mut self, board: &Board) {
        // The position is already fully evaluated (likely in case of a drawn position),
        // we have nothing more to do
        if self.full_eval { return; }

        let mut scores: [Score; 2] = [0; 2];

        // Mobility evaluation
        // TODO

        // Center control evaluation
        let (w_attack_map, b_attack_map) = (board.attack_map(Color::White, false), board.attack_map(Color::Black, false));
        let (w_safe_squares, b_safe_squares) = (w_attack_map & !b_attack_map, b_attack_map & !w_attack_map);
        let (w_center_control, b_center_control) = (w_safe_squares & Bitboard::CENTER, b_safe_squares & Bitboard::CENTER);
        let (w_large_center_control, b_large_center_control) = (w_safe_squares & Bitboard::LARGE_CENTER, b_safe_squares & Bitboard::LARGE_CENTER);
        scores[Color::White as usize] += w_center_control.pop_count() as i32 * 5 + w_large_center_control.pop_count() as i32 * 2;
        scores[Color::Black as usize] += b_center_control.pop_count() as i32 * 5 + b_large_center_control.pop_count() as i32 * 2;

        self.score += scores[board.side_to_move() as usize] - scores[board.side_to_move().opposite() as usize];
        self.full_eval = true;
    }

    /// Checks if a position is drawn
    pub fn is_drawn(board: &Board) -> bool {
        if board.is_drawn() { return true }

        let occupancy = board.get_occupancy_bitboard();
        match occupancy.pop_count() {
            2 => true,
            3 => {
                let other = board
                    .piece_type_on(
                        (board.get_occupancy_bitboard() & !board.get_piecetype_bitboard(PieceType::King)).ls1b().unwrap()
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
    pub fn see(board: &Board, m: Move) -> Score {
        if !m.is_capture() { return 0; } // The move cannot be evaluated by SEE

        let mut origin = m.origin();
        let target = m.target();

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
        gain[0] = Self::MIDGAME_PIECE_TYPE_VALUE[attacked as usize];

        // Next captures value
        loop {
            current_depth += 1;
            attacking_side = attacking_side.opposite();

            gain[current_depth] = Self::MIDGAME_PIECE_TYPE_VALUE[attacker as usize] - gain[current_depth - 1]; // Score if the square is defended
            if max(-gain[current_depth - 1], gain[current_depth]) < 0 { break; } // We can choose to not recapture

            dealt_with.set(origin);
            attackers = board.attackers_of_square(target, attacking_side, dealt_with) & !dealt_with;

            origin = match Self::least_valuable_attacker_square(board, attackers, attacking_side) {
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
            for bb in board.material_iter(attacking_side) {
                let intersection = *bb & attackers_bb;
                if let Some(sq) = intersection.ls1b() { return Some(sq) }
            }
        }
        None
    }
}
