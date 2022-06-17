use crate::{
    r#move::Move, 
    board::Board, 
    piece::{Color, PieceType}, 
    bitboard::Bitboard, 
    square::Square,
    movelist::MoveList
};

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum GenType {
    Legal, 
    Captures
}

struct MoveGenInfo {
    pub move_list: MoveList,
    pub king_square: Square,
    pub side: Color,
    pub occupancy: Bitboard,
    pub opponents: Bitboard,
    pub push_targets: Bitboard,
    pub capture_targets: Bitboard,
    pub pinned_pieces: Bitboard,
    pub pinning_pieces: Bitboard,
}

/// Generates a subset of all legal moves for a given position
pub fn generate(board: &Board, gen_type: GenType) -> MoveList {
    let mut info = MoveGenInfo {
        move_list: MoveList::default(),
        king_square: if let Some(sq) = board.king_square(board.side_to_move()) { sq } else { return MoveList::default() },
        side: board.side_to_move(),
        occupancy: board.get_occupancy_bitboard(),
        opponents: board.get_color_bitboard(board.side_to_move().opposite()),
        push_targets: if gen_type == GenType::Captures { Bitboard::EMPTY } else { !board.get_occupancy_bitboard() },
        capture_targets: board.get_color_bitboard(board.side_to_move().opposite()),
        pinned_pieces: Bitboard::EMPTY,
        pinning_pieces: Bitboard::EMPTY,
    };

    let (pinned_bb, pinners_bb) = board.pins(info.king_square);
    info.pinned_pieces = pinned_bb;
    info.pinning_pieces = pinners_bb;

    let mut king_attackers = board.attackers_of_square(info.king_square, info.side.opposite(), Bitboard::EMPTY);
    let xray_attack_map = board.attack_map(info.side.opposite(), true);
    king_moves(xray_attack_map, &mut info);

    if let Some(checker_square) = king_attackers.next() {
        // The king is in double check, no other moves are legal so we can just return the king moves
        if king_attackers.next().is_some() { return info.move_list }
        
        // We update our masks to reflect our only available target squares
        info.capture_targets = Bitboard::from_square(checker_square);
        if gen_type != GenType::Captures && board.piece_type_on(checker_square).unwrap().can_slide() {
            info.push_targets = Bitboard::get_ray(info.king_square, checker_square) & !info.occupancy;
        } else {
            info.push_targets = Bitboard::EMPTY;
        }
    } else {
        // If not in check, we can check for castling moves
        castling(board, xray_attack_map, &mut info);
    }
    pawn_moves(board, &mut info);
    knight_moves(board, &mut info);
    slider_moves(board, &mut info);
    en_passant(board, &mut info);
    pinned_pieces_moves(board, &mut info);

    info.move_list
}

fn pawn_moves(board: &Board, info: &mut MoveGenInfo) {
    let (m, pre_promo_rank) = if info.side == Color::Black { (-1, Bitboard::RANKS[1]) } else { (1, Bitboard::RANKS[6]) };
    let mut pawns_bb = board.get_piece_bitboard(PieceType::Pawn, info.side) & !info.pinned_pieces;
    if !(pawns_bb & pre_promo_rank).is_empty() {
        pawn_promotions(board, info)
    }
    pawns_bb &= !pre_promo_rank;

    for target in Bitboard::pawn_pushes(pawns_bb, !info.occupancy, info.side) & info.push_targets {
        info.move_list.push(Move::new_quiet(target - (8*m as usize), target))
    }
    for target in Bitboard::pawn_double_pushes(pawns_bb, !info.occupancy, info.side) & info.push_targets {
        info.move_list.push(Move::new_double_push(target - (16*m as usize), target))
    }
    for target in Bitboard::pawn_west_attacks(pawns_bb, info.side) & info.capture_targets {
        info.move_list.push(Move::new_capture(target - (7*m as usize), target))
    }
    for target in Bitboard::pawn_east_attacks(pawns_bb, info.side) & info.capture_targets {
        info.move_list.push(Move::new_capture(target - (9*m as usize), target))
    }
}

fn pawn_promotions(board: &Board, info: &mut MoveGenInfo) {
    let (m, pre_promo_rank) = if info.side == Color::Black { (-1, Bitboard::RANKS[1]) } else { (1, Bitboard::RANKS[6]) };
    let capture_mask = info.opponents & info.capture_targets;
    let pawns_bb = board.get_piece_bitboard(PieceType::Pawn, info.side) & !info.pinned_pieces & pre_promo_rank;

    for target in Bitboard::pawn_pushes(pawns_bb, !info.occupancy, info.side) & info.push_targets {
        Move::all_promotions(target - (8*m as usize), target)
            .map(|prom| info.move_list.push(prom));
    }
    for target in Bitboard::pawn_west_attacks(pawns_bb, info.side) & capture_mask {
        Move::all_promotion_captures(target - (7*m as usize), target)
            .map(|prom| info.move_list.push(prom));
    }
    for target in Bitboard::pawn_east_attacks(pawns_bb, info.side) & capture_mask {
        Move::all_promotion_captures(target - (9*m as usize), target)
            .map(|prom| info.move_list.push(prom));
    }
}

fn knight_moves(board: &Board, info: &mut MoveGenInfo) {
    let knights_bb = board.get_piece_bitboard(PieceType::Knight, info.side) & !info.pinned_pieces;
    for origin in knights_bb {
        let moves = Bitboard::KNIGHT_ATTACKS[origin];
        for target in moves & info.push_targets {
            info.move_list.push(Move::new_quiet(origin, target)) 
        }
        for target in moves & info.capture_targets {
            info.move_list.push(Move::new_capture(origin, target))
        }
    }
}

fn king_moves(attacks: Bitboard, info: &mut MoveGenInfo) {
    let moves = Bitboard::KING_ATTACKS[info.king_square] & !attacks;

    for target in moves & info.push_targets {
        info.move_list.push(Move::new_quiet(info.king_square, target))
    }
    for target in moves & info.capture_targets {
        info.move_list.push(Move::new_capture(info.king_square, target))
    }
}

fn slider_moves(board: &Board, info: &mut MoveGenInfo) {
    let diagonal_sliders = board.get_diagonal_sliders_bitboard(info.side) & !info.pinned_pieces;
    let cardinal_sliders = board.get_cardinal_sliders_bitboard(info.side) & !info.pinned_pieces;

    for origin in diagonal_sliders {
        let moves = Bitboard::bishop_attacks(origin, info.occupancy);
        for target in moves & info.push_targets {
            info.move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            info.move_list.push(Move::new_capture(origin, target));
        }
    }
    for origin in cardinal_sliders {
        let moves = Bitboard::rook_attacks(origin, info.occupancy);
        for target in moves & info.push_targets {
            info.move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            info.move_list.push(Move::new_capture(origin, target));
        }
    }
}

/// Mainly used for pinned pieces
fn specific_slider_moves(board: &Board, piece_type: PieceType, info: &mut MoveGenInfo) {
    let bb = board.get_piece_bitboard(piece_type, board.side_to_move()) & !info.pinned_pieces;

    for origin in bb {
        let moves = Bitboard::slider_attacks(piece_type, origin, info.occupancy);
        for target in moves & info.push_targets {
            info.move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            info.move_list.push(Move::new_capture(origin, target));
        }
    }
}

fn piece_move(board: &Board, piece_type: PieceType, info: &mut MoveGenInfo) {
    match piece_type {
        PieceType::Pawn => pawn_moves(board, info),
        PieceType::Knight => knight_moves(board, info),
        PieceType::Bishop | PieceType::Rook | PieceType::Queen => specific_slider_moves(board, piece_type, info),
        _ => ()
    }
}

fn en_passant(board: &Board, info: &mut MoveGenInfo) {
    if let Some(target) = board.en_passant_target() {
        let removed_piece_square = if board.side_to_move() == Color::White { target - 8 } else { target + 8 };
        if !info.push_targets.is_set(target) && !info.capture_targets.is_set(removed_piece_square) { return }

        // Checks for extermely rare cases where the en passant capture might leave the king in check
        let rook_sliders = board.get_piece_bitboard(PieceType::Rook, board.side_to_move().opposite()) | board.get_piece_bitboard(PieceType::Queen, board.side_to_move().opposite());
        let occupancy = info.occupancy & !Bitboard::from_square(removed_piece_square);
        let mut cross_ray = Bitboard::EMPTY;
        for origin in rook_sliders {
            let pin_ray = Bitboard::ORIGIN_TARGET_RAYS[info.king_square][origin];
            let from_pinner = pin_ray & Bitboard::rook_attacks(origin, occupancy);
            let to_pinner = pin_ray & Bitboard::rook_attacks(info.king_square, occupancy);
            cross_ray |= to_pinner & from_pinner
        }

        let origins = 
            Bitboard::pawn_attacks(Bitboard::from_square(target), board.side_to_move().opposite()) 
            & board.get_piece_bitboard(PieceType::Pawn, board.side_to_move())
            & !info.pinned_pieces & !cross_ray;
        for origin in origins {
            info.move_list.push(Move::new_en_passant(origin, target))
        }
    }
}

fn castling(board: &Board, attacks: Bitboard, info: &mut MoveGenInfo) {
    let (king_side_right, queen_side_right) = board.side_to_move_castling_rights();
    let kingside_occupancy = info.occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[info.side as usize][0];
    let kingside_attacked = attacks & Bitboard::CASTLING_ATTACKED_MASKS[info.side as usize][0];
    let queenside_occupancy = info.occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[info.side as usize][1];
    let queenside_attacked = attacks & Bitboard::CASTLING_ATTACKED_MASKS[info.side as usize][1];

    if king_side_right && (kingside_occupancy | kingside_attacked).is_empty() {
        info.move_list.push(Move::new_kingside_castle(info.side))
    }
    if queen_side_right && (queenside_occupancy | queenside_attacked).is_empty() {
        info.move_list.push(Move::new_queenside_castle(info.side))
    }
} 

fn pinned_pieces_moves(board: &Board, info: &mut MoveGenInfo) {
    let (push_targets, capture_targets, pinned_pieces) = (info.push_targets, info.capture_targets, info.pinned_pieces);
    for pinner_sq in info.pinning_pieces {
        let push_ray = Bitboard::get_ray(info.king_square, pinner_sq);
        let pinned_sq = (push_ray & pinned_pieces).ls1b().unwrap();
        info.pinned_pieces = !Bitboard::from_square(pinned_sq);
        info.push_targets = push_ray & !info.occupancy & push_targets;
        info.capture_targets = push_ray & info.opponents & capture_targets;
        let piece_type = board.piece_type_on(pinned_sq).unwrap();
        piece_move(board, piece_type, info);
    };
}

#[cfg(test)]
mod perft_tests {
    use crate::board::Board;
    use super::{generate, GenType};

    // Verification goes up to depth 4, more would be pointless as it would make
    // testing exponentially slower and the positions are varied enough to cover
    // all kinds of moves by depth 4 anyway
    const TEST_POSITIONS: [(&str, [u128; 4]); 7] = [
        (
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            [20, 400, 8902, 197281]
        ),
        (
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            [48, 2039, 97862, 4085603]
        ),
        (
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
            [14, 191, 2812, 43238]
        ),
        (
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            [6, 264, 9467, 422333]
        ),
        (
            "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
            [6, 264, 9467, 422333]
        ),
        (
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
            [44, 1486, 62379, 2103487]
        ),
        (
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
            [46, 2079, 89890, 3894594]
        ),
    ];

    #[test]
    fn perft_verification() {
        for (fen, results) in TEST_POSITIONS {
            let mut board = Board::new(fen);
            for d in 1..=4 {
                assert_eq!(results[d-1], perft(&mut board, d))
            }
        }
    }

    // perft with counting at horizon nodes
    fn perft(board: &mut Board, depth: usize) -> u128 {
        if depth == 0 { return 1 }
        let moves = generate(board, GenType::Legal);
        if depth == 1 { return moves.len() as u128 }

        let mut nodes = 0;
        for m in &moves {
            board.make(*m);
            nodes += perft(board, depth - 1);
            board.unmake();
        }
        nodes
    }
}
