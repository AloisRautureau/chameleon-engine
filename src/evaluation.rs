use crate::bitboard::Bitboard;
use crate::board::Board;
use crate::piece::Color;
use crate::piece::PieceType;
use crate::square::Square;

use crate::r#move::Move;
use std::cmp::max;

// Build script to calculate evaluation constants at compile time
include!(concat!(env!("OUT_DIR"), "/evaluation_constants.rs"));

pub type Score = i32;
pub enum GamePhase {
    Opening,
    MiddleGame,
    EndGame,
}

pub struct Evaluation {
    pub score: Score,
    pub game_phase: GamePhase,
    pub is_drawn: bool,
    pub full_eval: bool,
}
impl Evaluation {
    pub const MIDGAME_PIECE_TYPE_VALUE: [Score; 6] = [100, 300, 350, 500, 900, Self::MATE_SCORE - 1];
    pub const ENDGAME_PIECE_TYPE_VALUE: [Score; 6] = [120, 250, 300, 550, 850, Self::MATE_SCORE - 1];
    pub const MATE_SCORE: Score = i32::MAX / 2;
    pub const DRAW_SCORE: Score = 0;
    pub const PHASE_VALUE: [Score; 6] = [0, 1, 1, 2, 4, 0];
    pub const MAX_PHASE: Score = 24;

    /// Performs a simple, but quick evaluation of a position
    pub fn shallow_eval(board: &Board) -> Evaluation {
        if Self::is_drawn(board) {
            return Evaluation {
                score: Self::DRAW_SCORE,
                game_phase: GamePhase::EndGame,
                is_drawn: true,
                full_eval: true,
            };
        }

        let mut phase = 0;
        let mut mg_scores = [0, 0];
        let mut eg_scores = [0, 0];

        for (piece, bb) in board.material_iter(Color::White).enumerate() {
            for sq in *bb {
                phase += Self::PHASE_VALUE[piece];
                mg_scores[Color::White as usize] +=
                    Self::MIDGAME_PIECE_TYPE_VALUE[piece] + MIDGAME_PIECE_SQUARE_TABLE[piece][sq];
                eg_scores[Color::White as usize] +=
                    Self::ENDGAME_PIECE_TYPE_VALUE[piece] + ENDGAME_PIECE_SQUARE_TABLE[piece][sq];
            }
        }
        for (piece, bb) in board.material_iter(Color::Black).enumerate() {
            for sq in bb.vertical_flip() {
                phase += Self::PHASE_VALUE[piece];
                mg_scores[Color::Black as usize] +=
                    Self::MIDGAME_PIECE_TYPE_VALUE[piece] + MIDGAME_PIECE_SQUARE_TABLE[piece][sq];
                eg_scores[Color::Black as usize] +=
                    Self::ENDGAME_PIECE_TYPE_VALUE[piece] + MIDGAME_PIECE_SQUARE_TABLE[piece][sq];
            }
        }

        let game_phase = if phase >= 20 {
            GamePhase::Opening
        } else if phase >= 4 {
            GamePhase::MiddleGame
        } else {
            GamePhase::Opening
        };
        phase = std::cmp::min(phase, Self::MAX_PHASE);
        let us_index = board.side_to_move() as usize;
        let mg_score = mg_scores[us_index] - mg_scores[us_index ^ 1];
        let eg_score = eg_scores[us_index] - eg_scores[us_index ^ 1];
        //println!("phase value {}, mg {}, eg {}", phase, mg_score, eg_score);

        Evaluation {
            score: ((mg_score * phase) + (eg_score * (Self::MAX_PHASE - phase))) / Self::MAX_PHASE,
            game_phase,
            is_drawn: false,
            full_eval: false,
        }
    }

    pub fn deep_eval(&mut self, board: &Board) {
        // The position is already fully evaluated (likely in case of a drawn position),
        // we have nothing more to do
        if self.full_eval {
            return;
        }

        let mut scores: [Score; 2] = [0; 2];

        // Opened files evaluation
        let mut open_files = Bitboard::EMPTY;
        let mut semi_open_files = [Bitboard::EMPTY, Bitboard::EMPTY];
        for file in Bitboard::FILES {
            if (board.get_piecetype_bitboard(PieceType::Pawn) & file).is_empty() {
                open_files |= file
            } else if (board.get_piecetype_bitboard(PieceType::Pawn) & file).single_populated() {
                if (board.get_piece_bitboard(PieceType::Pawn, Color::White) & file).is_empty() {
                    semi_open_files[Color::Black as usize] |= file
                }
                if (board.get_piece_bitboard(PieceType::Pawn, Color::Black) & file).is_empty() {
                    semi_open_files[Color::White as usize] |= file
                }
            }
        }
        // Rook on open/semi-open file
        scores[Color::White as usize] += (board.get_piece_bitboard(PieceType::Rook, Color::White)
            & open_files)
            .pop_count() as Score
            * 20
            + (board.get_piece_bitboard(PieceType::Rook, Color::White)
                & semi_open_files[Color::White as usize])
                .pop_count() as Score
                * 10;
        scores[Color::Black as usize] += (board.get_piece_bitboard(PieceType::Rook, Color::Black)
            & open_files)
            .pop_count() as Score
            * 20
            + (board.get_piece_bitboard(PieceType::Rook, Color::Black)
                & semi_open_files[Color::Black as usize])
                .pop_count() as Score
                * 10;

        // Bishop pair bonus
        scores[Color::White as usize] += (board
            .get_piece_bitboard(PieceType::Bishop, Color::White)
            .pop_count() as Score
            % 2
            + 1)
            * 30;
        scores[Color::Black as usize] += (board
            .get_piece_bitboard(PieceType::Bishop, Color::Black)
            .pop_count() as Score
            % 2
            + 1)
            * 30;

        // Mobility evaluation
        // TODO

        // Center control evaluation
        let (w_attack_map, b_attack_map) = (
            board.attack_map(Color::White, Bitboard::EMPTY),
            board.attack_map(Color::Black, Bitboard::EMPTY),
        );
        let (w_safe_squares, b_safe_squares) =
            (w_attack_map & !b_attack_map, b_attack_map & !w_attack_map);
        let (w_center_control, b_center_control) = (
            w_safe_squares & Bitboard::CENTER,
            b_safe_squares & Bitboard::CENTER,
        );
        let (w_large_center_control, b_large_center_control) = (
            w_safe_squares & Bitboard::LARGE_CENTER,
            b_safe_squares & Bitboard::LARGE_CENTER,
        );
        scores[Color::White as usize] +=
            w_center_control.pop_count() as Score * 5 + w_large_center_control.pop_count() as Score * 2;
        scores[Color::Black as usize] +=
            b_center_control.pop_count() as Score * 5 + b_large_center_control.pop_count() as Score * 2;

        self.score += scores[board.side_to_move() as usize]
            - scores[board.side_to_move().opposite() as usize];
        self.full_eval = true;
    }

    /// Checks if a position is drawn
    pub fn is_drawn(board: &Board) -> bool {
        if board.is_drawn() {
            return true;
        }

        let occupancy = board.get_occupancy_bitboard();
        match occupancy.pop_count() {
            2 => true,
            3 => {
                let other = board
                    .piece_type_on(
                        (board.get_occupancy_bitboard()
                            & !board.get_piecetype_bitboard(PieceType::King))
                        .ls1b()
                        .unwrap(),
                    )
                    .unwrap();
                other == PieceType::Knight || other == PieceType::Bishop
            }
            4 => {
                let white_others = board.get_color_bitboard(Color::White)
                    & !board.get_piecetype_bitboard(PieceType::King);
                let black_others = board.get_color_bitboard(Color::Black)
                    & !board.get_piecetype_bitboard(PieceType::King);
                white_others.pop_count() == black_others.pop_count()
            }
            _ => false,
        }
    }

    /// Simulates a capture move and returns an approximation of its value
    pub fn see(board: &Board, m: Move) -> Score {
        if !m.is_capture() {
            return 0;
        } // The move cannot be evaluated by SEE

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

            gain[current_depth] =
                Self::MIDGAME_PIECE_TYPE_VALUE[attacker as usize] - gain[current_depth - 1]; // Score if the square is defended
            if max(-gain[current_depth - 1], gain[current_depth]) < 0 {
                break;
            } // We can choose to not recapture

            dealt_with.set(origin);
            attackers = board.attackers_of_square(target, attacking_side, dealt_with) & !dealt_with;

            origin = match Self::least_valuable_attacker_square(board, attackers, attacking_side) {
                Some(sq) => sq,
                None => break,
            };
            attacker = board.piece_type_on(origin).unwrap();
        }

        while current_depth != 1 {
            current_depth -= 1;
            gain[current_depth - 1] = -max(-gain[current_depth - 1], gain[current_depth]);
        }
        gain[0]
    }

    /// Given a bitboard of attackers, returns the least valuable attacker from that bitboard
    fn least_valuable_attacker_square(
        board: &Board,
        attackers_bb: Bitboard,
        attacking_side: Color,
    ) -> Option<Square> {
        if attackers_bb != Bitboard::EMPTY {
            for bb in board.material_iter(attacking_side) {
                let intersection = *bb & attackers_bb;
                if let Some(sq) = intersection.ls1b() {
                    return Some(sq);
                }
            }
        }
        None
    }
}
