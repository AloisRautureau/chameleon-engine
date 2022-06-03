use crate::{
    r#move::Move, 
    board::Board, 
    piece::{Color, PieceType}, 
    bitboard::Bitboard, 
    movelist::MoveList
};
use crate::square::Square;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum GenType {
    Legal, 
    Captures
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
struct MoveGenInfo {
    pub king_square: Square,
    pub side: Color,
    pub occupancy: Bitboard,
    pub opponents: Bitboard,
    pub push_targets: Bitboard,
    pub capture_targets: Bitboard,
    pub pinned_pieces: Bitboard,
    pub pinning_pieces: Bitboard,

    pub gen_type: GenType
}

/// Generates a subset of all legal moves for a given position
pub fn generate(board: &Board, gen_type: GenType) -> MoveList {
    let mut move_list = Default::default();
    let mut info = MoveGenInfo {
        king_square: if let Some(sq) = board.king_square(board.side_to_move()) { sq } else { return move_list },
        side: board.side_to_move(),
        occupancy: board.get_occupancy_bitboard(),
        opponents: board.get_color_bitboard(board.side_to_move().opposite()),
        push_targets: if gen_type == GenType::Captures { Bitboard::EMPTY } else { !board.get_occupancy_bitboard() },
        capture_targets: board.get_color_bitboard(board.side_to_move().opposite()),
        pinned_pieces: Bitboard::EMPTY,
        pinning_pieces: Bitboard::EMPTY,

        gen_type
    };

    let (pinned_bb, pinners_bb) = board.pins(info.king_square);
    info.pinned_pieces = pinned_bb;
    info.pinning_pieces = pinners_bb;

    let king_attackers = board.attackers_of_square(info.king_square, info.side.opposite(), Bitboard::EMPTY);
    let xray_attack_map = board.attack_map(info.side.opposite(), true);
    king_moves(&mut move_list, xray_attack_map, &info);

    let checkers_count = king_attackers.pop_count();
    if checkers_count <= 1 {
        if checkers_count == 1 {
            let checker_square = king_attackers.ls1b();
            info.capture_targets = king_attackers;

            if gen_type != GenType::Captures && board.piece_type_on(checker_square).unwrap().can_slide() {
                info.push_targets = Bitboard::get_ray(info.king_square, checker_square) & !info.occupancy;
            } else {
                info.push_targets = Bitboard::EMPTY
            }
        }

        pawn_moves(board, &mut move_list, &info);
        knight_moves(board, &mut move_list, &info);
        slider_moves(board, &mut move_list, &info);

        en_passant(board, &mut move_list, &info);
        if checkers_count == 0 && gen_type != GenType::Captures { castling(board, &mut move_list, xray_attack_map, &info) }
    }

    pinned_pieces_moves(board, &mut move_list, &info);

    move_list
}

fn pawn_moves(board: &Board, move_list: &mut MoveList, info: &MoveGenInfo) {
    let pawns_bb = board.get_piece_bitboard(PieceType::Pawn, board.side_to_move()) & !info.pinned_pieces;
    let (push_shift, west_shift, east_shift, promo_rank) = {
        if board.side_to_move() == Color::White { 
            (8, 7, 9, Bitboard::RANKS[7]) 
        } else {
            (-8, -9, -7, Bitboard::RANKS[0]) 
        }
    };

    for target in Bitboard::pawn_pushes(pawns_bb, !info.occupancy, board.side_to_move()) & info.push_targets {
        if promo_rank.is_set(target) { 
            Move::all_promotions(target - (push_shift as usize), target)
                .map(|prom| move_list.push(prom));
        } else {
            move_list.push(Move::new_quiet(target - (push_shift as usize), target))
        }
    }
    for target in Bitboard::pawn_double_pushes(pawns_bb, !info.occupancy, board.side_to_move()) & info.push_targets {
        move_list.push(Move::new_double_push(target - (2*push_shift as usize) , target))
    }
    for target in Bitboard::pawn_west_attacks(pawns_bb, board.side_to_move()) & info.opponents & info.capture_targets  {
        if promo_rank.is_set(target) { 
            Move::all_promotions(target - (west_shift as usize), target)
                .map(|prom| move_list.push(prom));
        } else {
            move_list.push(Move::new_capture(target - (west_shift as usize), target))
        }
    }
    for target in Bitboard::pawn_east_attacks(pawns_bb, board.side_to_move()) & info.opponents & info.capture_targets  {
        if promo_rank.is_set(target) { 
            Move::all_promotion_captures(target - (east_shift as usize), target)
                .map(|prom| move_list.push(prom));
        } else {
            move_list.push(Move::new_capture(target - (east_shift as usize), target))
        }
    }
}

fn knight_moves(board: &Board, move_list: &mut MoveList, info: &MoveGenInfo) {
    let knights_bb = board.get_piece_bitboard(PieceType::Knight, board.side_to_move()) & !info.pinned_pieces;
    for origin in knights_bb {
        let moves = Bitboard::KNIGHT_ATTACKS[origin];
        for target in moves & info.push_targets {
            move_list.push(Move::new_quiet(origin, target)) 
        }
        for target in moves & info.capture_targets {
            move_list.push(Move::new_capture(origin, target))
        }
    }
}

fn king_moves(move_list: &mut MoveList, attacks: Bitboard, info: &MoveGenInfo) {
    let moves = Bitboard::KING_ATTACKS[info.king_square] & !attacks;

    for target in moves & info.push_targets {
        move_list.push(Move::new_quiet(info.king_square, target))
    }
    for target in moves & info.capture_targets {
        move_list.push(Move::new_capture(info.king_square, target))
    }
}

fn slider_moves(board: &Board, move_list: &mut MoveList, info: &MoveGenInfo) {
    let bishop_bb = board.get_piece_bitboard(PieceType::Bishop, info.side);
    let rook_bb = board.get_piece_bitboard(PieceType::Rook, info.side);
    let queen_bb = board.get_piece_bitboard(PieceType::Queen, info.side);
    let diagonal_sliders = (queen_bb | bishop_bb) & !info.pinned_pieces;
    let cardinal_sliders = (queen_bb | rook_bb) & !info.pinned_pieces;

    for origin in diagonal_sliders {
        let moves = Bitboard::bishop_attacks(origin, info.occupancy);
        for target in moves & info.push_targets {
            move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            move_list.push(Move::new_capture(origin, target));
        }
    }
    for origin in cardinal_sliders {
        let moves = Bitboard::rook_attacks(origin, info.occupancy);
        for target in moves & info.push_targets {
            move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            move_list.push(Move::new_capture(origin, target));
        }
    }
}

/// Mainly used for pinned pieces
fn specific_slider_moves(board: &Board, move_list: &mut MoveList, piece_type: PieceType, info: &MoveGenInfo) {
    let bb = board.get_piece_bitboard(piece_type, board.side_to_move()) & !info.pinned_pieces;

    for origin in bb {
        let moves = Bitboard::slider_attacks(piece_type, origin, info.occupancy);
        for target in moves & info.push_targets {
            move_list.push(Move::new_quiet(origin, target));
        }
        for target in moves & info.capture_targets {
            move_list.push(Move::new_capture(origin, target));
        }
    }
}

fn piece_move(board: &Board, move_list: &mut MoveList, piece_type: PieceType, info: &MoveGenInfo) {
    match piece_type {
        PieceType::Pawn => pawn_moves(board, move_list, info),
        PieceType::Knight => knight_moves(board, move_list, info),
        PieceType::Bishop | PieceType::Rook | PieceType::Queen => specific_slider_moves(board, move_list, piece_type, info),
        _ => ()
    }
}

fn en_passant(board: &Board, move_list: &mut MoveList, info: &MoveGenInfo) {
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
            move_list.push(Move::new_en_passant(origin, target))
        }
    }
}

fn castling(board: &Board, move_list: &mut MoveList, attacks: Bitboard, info: &MoveGenInfo) {
    let (king_side_right, queen_side_right) = board.side_to_move_castling_rights();
    let kingside_occupancy = info.occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[board.side_to_move() as usize][0];
    let kingside_attacked = attacks & Bitboard::CASTLING_ATTACKED_MASKS[board.side_to_move() as usize][0];
    let queenside_occupancy = info.occupancy & Bitboard::CASTLING_OCCUPANCY_MASKS[board.side_to_move() as usize][1];
    let queenside_attacked = attacks & Bitboard::CASTLING_ATTACKED_MASKS[board.side_to_move() as usize][1];

    if king_side_right && kingside_occupancy == Bitboard::EMPTY && kingside_attacked == Bitboard::EMPTY {
        move_list.push(Move::new_kingside_castle(board.side_to_move() == Color::White))
    }
    if queen_side_right && queenside_occupancy == Bitboard::EMPTY && queenside_attacked == Bitboard::EMPTY {
        move_list.push(Move::new_queenside_castle(board.side_to_move() == Color::White))
    }
} 

fn pinned_pieces_moves(board: &Board, move_list: &mut MoveList, info: &MoveGenInfo) {
    let mut pinned_gen_info = *info;
    for pinner_sq in info.pinning_pieces {
        let push_ray = Bitboard::get_ray(info.king_square, pinner_sq);
        let pinned_sq = (push_ray & info.pinned_pieces).ls1b();
        pinned_gen_info.pinned_pieces = !Bitboard::from_square(pinned_sq);
        pinned_gen_info.push_targets = push_ray & !info.occupancy & info.push_targets;
        pinned_gen_info.capture_targets = push_ray & info.opponents & info.capture_targets;
        let piece_type = board.piece_type_on(pinned_sq).unwrap();
        piece_move(board, move_list, piece_type, &pinned_gen_info);
    };
}