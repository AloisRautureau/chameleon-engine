use crate::{
    bitboard::Bitboard,
    board::Board,
    movelist::MoveList,
    piece::{Color, PieceType},
    r#move::Move,
};

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
pub enum GenType {
    Legal,
    Captures,
}

pub fn generate(board: &Board, gen_type: GenType) -> MoveList {
    let mut move_list = MoveList::default();

    let us = board.side_to_move();
    let them = us.opposite();
    let us_index = us as usize;

    let king_sq = if let Some(sq) = board.king_square(us) {
        sq
    } else {
        return move_list;
    };
    let occupancy = board.get_occupancy_bitboard();
    let opponents = board.get_color_bitboard(them);

    let mut push_mask = if gen_type == GenType::Captures {
        Bitboard::EMPTY
    } else {
        !occupancy
    };
    let mut capture_mask = opponents;
    let (pinned_mask, pinning_mask) = board.pins(king_sq);

    let pre_promo_rank = Bitboard::RANKS[1] << (40 * us_index);
    let dpush_rank = Bitboard::RANKS[5] >> (24 * us_index);
    let shift_mult = if us == Color::White { 1 } else { -1 };

    // Gets a bitboard of checkers, as well as every attacked square (xraying our king)
    let checkers = board.attackers_of_square(king_sq, them, Bitboard::EMPTY);
    let attack_map = board.attack_map(them, Bitboard::from_square(king_sq));
    let king_moves = Bitboard::KING_ATTACKS[king_sq] & !attack_map;
    for t in king_moves & push_mask {
        move_list.push(Move::new_quiet(king_sq, t))
    }
    for t in king_moves & capture_mask {
        move_list.push(Move::new_capture(king_sq, t))
    }

    // Depending on our king's situation (check, double check, safe), we may
    // generate different subsets of pseudo-legal moves
    if checkers.more_than_one_set() {
        return move_list;
    } else if checkers.single_populated() {
        // In single check, we can only capture the checker or block its ray
        push_mask = if gen_type == GenType::Captures {
            Bitboard::EMPTY
        } else {
            Bitboard::get_ray(king_sq, checkers.ls1b().unwrap()) & !occupancy
        };
        capture_mask = checkers;
    } else {
        // If we're not in check at all, we can generate castling moves
        // as well as pinned pieces moves
        let (kingside_ok, queenside_ok) = board.side_to_move_castling_rights();
        if kingside_ok
            && ((occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[us_index][0])
                | (attack_map & Bitboard::CASTLING_ATTACKED_MASKS[us_index][0]))
                .is_empty()
        {
            move_list.push(Move::new_kingside_castle(us))
        }
        if queenside_ok
            && ((occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[us_index][1])
                | (attack_map & Bitboard::CASTLING_ATTACKED_MASKS[us_index][1]))
                .is_empty()
        {
            move_list.push(Move::new_queenside_castle(us))
        }

        // PINNED PIECES
        for pinner_sq in pinning_mask {
            let pin_ray = Bitboard::get_ray(king_sq, pinner_sq);
            let pinned_sq = (pinned_mask & pin_ray).ls1b().unwrap();
            let attacks = match board.piece_type_on(pinned_sq).unwrap() {
                PieceType::Pawn => {
                    let bb = Bitboard::from_square(pinned_sq);
                    let cap = (Bitboard::generalized_shift(&bb, 9 * shift_mult)
                        & !Bitboard::FILES[7 - 7 * us_index])
                        | (Bitboard::generalized_shift(&bb, 7 * shift_mult)
                            & !Bitboard::FILES[7 * us_index]);
                    let push = Bitboard::generalized_shift(&bb, 8 * shift_mult) & !occupancy;
                    let dpush = Bitboard::generalized_shift(&(push & dpush_rank), 8 * shift_mult)
                        & !occupancy;
                    for target in push & pin_ray {
                        move_list.push(Move::new_quiet(pinned_sq, target))
                    }
                    for target in dpush & pin_ray {
                        move_list.push(Move::new_double_push(pinned_sq, target))
                    }
                    if cap.is_set(pinner_sq) {
                        if pre_promo_rank.is_set(pinned_sq) {
                            Move::all_promotion_captures(pinned_sq, pinner_sq)
                                .map(|prom| move_list.push(prom));
                        } else {
                            move_list.push(Move::new_capture(pinned_sq, pinner_sq))
                        }
                    }
                    if (cap & pin_ray).is_set(board.en_passant_target().unwrap_or(64)) {
                        move_list.push(Move::new_en_passant(
                            pinned_sq,
                            board.en_passant_target().unwrap(),
                        ))
                    }
                    Bitboard::EMPTY
                }
                PieceType::Bishop => Bitboard::bishop_attacks(pinned_sq, occupancy),
                PieceType::Rook => Bitboard::rook_attacks(pinned_sq, occupancy),
                PieceType::Queen => {
                    Bitboard::bishop_attacks(pinned_sq, occupancy)
                        | Bitboard::rook_attacks(pinned_sq, occupancy)
                }
                _ => Bitboard::EMPTY,
            };
            for target in attacks & pin_ray & push_mask {
                move_list.push(Move::new_quiet(pinned_sq, target))
            }
            if attacks.is_set(pinner_sq) {
                move_list.push(Move::new_capture(pinned_sq, pinner_sq))
            }
        }
    }
    let capture_mask = capture_mask;
    let push_mask = push_mask;

    // We then generate each piece's moves using our masks

    // PAWNS
    let mut pawns = board.get_piece_bitboard(PieceType::Pawn, us) & !pinned_mask;
    let promoting_pawns = pawns & pre_promo_rank;
    pawns ^= promoting_pawns;
    let single_push = Bitboard::generalized_shift(&pawns, 8 * shift_mult) & !occupancy;
    for target in single_push & push_mask {
        move_list.push(Move::new_quiet(target + 8 - 16 * us_index, target))
    }
    for target in
        Bitboard::generalized_shift(&(single_push & dpush_rank), 8 * shift_mult) & push_mask
    {
        move_list.push(Move::new_double_push(target + 16 - 32 * us_index, target))
    }
    for target in Bitboard::generalized_shift(&pawns, 7 * shift_mult)
        & !Bitboard::FILES[7 * us_index]
        & capture_mask
    {
        move_list.push(Move::new_capture(target + 7 - 14 * us_index, target))
    }
    for target in Bitboard::generalized_shift(&pawns, 9 * shift_mult)
        & !Bitboard::FILES[7 - 7 * us_index]
        & capture_mask
    {
        move_list.push(Move::new_capture(target + 9 - 18 * us_index, target))
    }
    for target in Bitboard::generalized_shift(&promoting_pawns, 8 * shift_mult) & push_mask {
        Move::all_promotions(target + 8 - 16 * us_index, target).map(|prom| move_list.push(prom));
    }
    for target in Bitboard::generalized_shift(&promoting_pawns, 7 * shift_mult)
        & !Bitboard::FILES[7 * us_index]
        & capture_mask
    {
        Move::all_promotions(target + 7 - 14 * us_index, target).map(|prom| move_list.push(prom));
    }
    for target in Bitboard::generalized_shift(&promoting_pawns, 9 * shift_mult)
        & !Bitboard::FILES[7 - 7 * us_index]
        & capture_mask
    {
        Move::all_promotions(target + 9 - 18 * us_index, target).map(|prom| move_list.push(prom));
    }
    if let Some(ep_sq) = board.en_passant_target() {
        let ep_bb = Bitboard::from_square(ep_sq);
        let captured_bb = Bitboard::generalized_shift(&ep_bb, 8 * -shift_mult);
        let origin_bb = ((Bitboard::generalized_shift(&ep_bb, 7 * -shift_mult)
            & !Bitboard::FILES[7 - 7 * us_index])
            | (Bitboard::generalized_shift(&ep_bb, 9 * -shift_mult)
                & !Bitboard::FILES[7 * us_index]))
            & pawns;

        // In rare cases, taking en passant might leave the king open to a check
        let discovered_rank = Bitboard::RANKS[crate::square::rank_of(captured_bb.ls1b().unwrap())];
        let pinned_ep = !origin_bb.more_than_one_set()
            && discovered_rank.is_set(king_sq)
            && !board
                .attackers_of_square(king_sq, them, origin_bb | captured_bb)
                .is_empty();
        let in_capture_push_mask = !((ep_bb & push_mask) | (captured_bb & capture_mask)).is_empty();
        if !origin_bb.is_empty() && in_capture_push_mask && !pinned_ep {
            for origin in origin_bb {
                move_list.push(Move::new_en_passant(origin, ep_sq))
            }
        }
    }

    // KNIGHTS
    for origin in board.get_piece_bitboard(PieceType::Knight, us) & !pinned_mask {
        let attacks = Bitboard::KNIGHT_ATTACKS[origin];
        for target in attacks & push_mask {
            move_list.push(Move::new_quiet(origin, target))
        }
        for target in attacks & capture_mask {
            move_list.push(Move::new_capture(origin, target))
        }
    }

    // DIAGONAL SLIDERS
    for origin in board.get_diagonal_sliders_bitboard(us) & !pinned_mask {
        let attacks = Bitboard::bishop_attacks(origin, occupancy);
        for target in attacks & push_mask {
            move_list.push(Move::new_quiet(origin, target))
        }
        for target in attacks & capture_mask {
            move_list.push(Move::new_capture(origin, target))
        }
    }

    // CARDINAL SLIDERS
    for origin in board.get_cardinal_sliders_bitboard(us) & !pinned_mask {
        let attacks = Bitboard::rook_attacks(origin, occupancy);
        for target in attacks & push_mask {
            move_list.push(Move::new_quiet(origin, target))
        }
        for target in attacks & capture_mask {
            move_list.push(Move::new_capture(origin, target))
        }
    }

    move_list
}

#[cfg(test)]
mod perft_tests {
    use super::{generate, GenType};
    use crate::board::Board;

    // Verification goes up to depth 5, more would be pointless as it would make
    // testing exponentially slower and the positions are varied enough to cover
    // all kinds of moves by depth 4 anyway
    const TEST_POSITIONS: [(&str, [u128; 5]); 7] = [
        (
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            [20, 400, 8902, 197281, 4865609],
        ),
        (
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            [48, 2039, 97862, 4085603, 193690690],
        ),
        (
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
            [14, 191, 2812, 43238, 674624],
        ),
        (
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            [6, 264, 9467, 422333, 15833292],
        ),
        (
            "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
            [6, 264, 9467, 422333, 15833292],
        ),
        (
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
            [44, 1486, 62379, 2103487, 89941194],
        ),
        (
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
            [46, 2079, 89890, 3894594, 164075551],
        ),
    ];

    #[test]
    fn perft_verification() {
        for (fen, results) in TEST_POSITIONS {
            let mut board = Board::new(fen);
            println!("{}\n", board);
            println!("move  nodes");
            for d in 1..=5 {
                let moves = generate(&board, GenType::Legal);
                let mut nodes = 0;
                for m in &moves {
                    board.make(*m);
                    let move_nodes = perft(&mut board, d - 1);
                    nodes += move_nodes;
                    println!("{}: {}", m, move_nodes);
                    board.unmake();
                }
                println!("nodes at depth {}: {}", d, nodes);
                assert_eq!(results[d - 1], nodes)
            }
            println!();
        }
    }

    // perft with counting at horizon nodes
    fn perft(board: &mut Board, depth: usize) -> u128 {
        if depth == 0 {
            return 1;
        }
        let moves = generate(board, GenType::Legal);
        if depth == 1 {
            return moves.len() as u128;
        }

        let mut nodes = 0;
        for m in &moves {
            board.make(*m);
            nodes += perft(board, depth - 1);
            board.unmake();
        }
        nodes
    }
}
