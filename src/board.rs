use std::fmt::{Display, Formatter};
use crate::bitboard::Bitboard;
use crate::castling::CastlingRights;
use crate::history::HistoryEntry;
use crate::piece::{Color, Piece, PieceType};
use crate::piece::Color::{Black, White};
use crate::piece::PieceType::Pawn;
use crate::r#move::{ Move, MoveFlags };
use crate::square::{parse_square, Square, square_representation};
use crate::zob_hash::{ZobristHasher, Hash};
use crate::move_generator::{ generate, GenType };

// Bitboards are indexed by color and piece_type, with a redudant
// color bitboard at index 7.

#[derive(Clone)]
pub struct Board {
    bitboards: [[Bitboard; 7]; 2],
    pieces: [Option<Piece>; 64],
    side_to_move: Color,
    castling_rights: CastlingRights,
    ep_target: Option<Square>,
    ply: u32,
    reversible_moves: u32,

    history_entries: Vec<HistoryEntry>,
    repeatable: Vec<Hash>, // Enables repetition detection
    hash: Hash,
}

impl Board {
    /// Creates a new board given its FEN representation
    pub fn new(fen: &str) -> Board {
        let mut b = Board {
            bitboards: [[Bitboard::EMPTY; 7]; 2],
            pieces: [None; 64],
            side_to_move: White,
            castling_rights: CastlingRights::new(),
            ep_target: None,
            ply: 0,
            reversible_moves: 0,

            history_entries: Vec::with_capacity(64),
            repeatable: Vec::with_capacity(16),
            hash: 0,
        };
        b.set_fen(fen);
        b
    }

    /// Makes a move on the board
    /// Note that the move is expected to be legal, inputting a random
    /// 16bit integer will break the position
    pub fn make(&mut self, mv: Move) {
        if mv == Move::NULL_MOVE { self.make_null(); return }

        let origin = mv.origin();
        let target = mv.target();

        let before_move_hash = self.hash;

        let history_entry = HistoryEntry {
            move_played: mv,
            captured_piece: self.remove_piece(target),
            ep_target: self.ep_target,
            castling_rights: self.castling_rights,
            reversible_moves: self.reversible_moves
        };

        self.hash ^= ZobristHasher::castling_rights_hash(self.castling_rights);
        if self.piece_type_on(origin) == Some(PieceType::King) {
            self.castling_rights.uncastle(self.side_to_move);
        }
        if origin == 7 || target == 7 {
            self.castling_rights.uncastle_kingside(Color::White);
        } else if origin == 0 || target == 0 {
            self.castling_rights.uncastle_queenside(Color::White);
        } else if origin == 63 || target == 63 {
            self.castling_rights.uncastle_kingside(Color::Black);
        } else if origin == 56 || target == 56 {
            self.castling_rights.uncastle_queenside(Color::Black);
        }
        self.hash ^= ZobristHasher::castling_rights_hash(self.castling_rights);

        self.hash ^= ZobristHasher::en_passant_hash(self.ep_target);
        self.ep_target = None;

        let moved_piece = self.remove_piece(origin).unwrap();
        self.add_piece(moved_piece, target);

        match mv.flags() {
            MoveFlags::Quiet if moved_piece.piece_type != PieceType::Pawn => {
                self.reversible_moves += 1;
                self.repeatable.push(before_move_hash);
            },
            MoveFlags::DoublePush => {
                self.ep_target = Some((target + origin) / 2);
                self.hash ^= ZobristHasher::en_passant_hash(self.ep_target);
            },
            MoveFlags::EnPassant => { 
                self.remove_piece(if self.side_to_move == White { target-8 } else { target+8 });
            },
            MoveFlags::Promotion(p) | MoveFlags::PromotionCapture(p) => {
                self.remove_piece(target);
                self.add_piece(Piece { piece_type: p, color: self.side_to_move }, target);
            },
            MoveFlags::KingSideCastle => {
                let moved_rook = self.remove_piece(if self.side_to_move == White { 7 } else { 63 }).unwrap();
                self.add_piece(moved_rook, if self.side_to_move == White { 5 } else { 61 } )
            },
            MoveFlags::QueenSideCastle => {
                let moved_rook = self.remove_piece(if self.side_to_move == White { 0 } else { 56 }).unwrap();
                self.add_piece(moved_rook, if self.side_to_move == White { 3 } else { 59 } )
            },
            _ => ()
        }
        
        self.hash ^= ZobristHasher::ZOBRIST_KEYS[ZobristHasher::BLACK_TO_MOVE_INDEX];
        self.side_to_move = self.side_to_move.opposite();
        self.ply += 1;
        if self.reversible_moves == history_entry.reversible_moves {
            self.reversible_moves = 0;
            self.repeatable = Vec::with_capacity(16);
        }

        self.history_entries.push(history_entry);
    }

    /// Given a string, makes the move if it is legal
    pub fn make_from_str(&mut self, move_str: &str) -> Result<(), String> {
        let legal_moves = generate(&self, GenType::Legal);
        let (origin, target, promotion_target) = if let Some(mv) = Move::parse(move_str) {
            mv
        } else {
            return Err(String::from("Move is not formatted correctly"))
        };

        if let Some(mv) = legal_moves.into_iter().find(|m| m.origin() == origin && m.target() == target && m.promotion_target() == promotion_target) {
            self.make(mv);
            Ok(())
        } else {
            Err(String::from("Illegal move"))
        }
    }

    /// Makes a null move (the side to move passes its turn). Only used in search
    fn make_null(&mut self) {
        let hist = HistoryEntry {
            move_played: Move::NULL_MOVE,
            ep_target: self.ep_target,
            castling_rights: self.castling_rights,
            reversible_moves: self.reversible_moves,
            captured_piece: None,
        };
        self.hash ^= ZobristHasher::en_passant_hash(self.ep_target);
        self.ep_target = None;
        self.hash ^= ZobristHasher::ZOBRIST_KEYS[ZobristHasher::BLACK_TO_MOVE_INDEX];
        self.side_to_move = self.side_to_move.opposite();
        self.ply += 1;

        self.history_entries.push(hist);
    }

    /// Unmakes the move on the top of the history stack
    pub fn unmake(&mut self) {
        let unretrievable_info = if let Some(h) = self.history_entries.pop() { h } else { return };
        self.repeatable.pop();

        let move_played = unretrievable_info.move_played;
        if move_played == Move::NULL_MOVE { self.unmake_null(unretrievable_info.ep_target); return }

        let captured_piece = unretrievable_info.captured_piece;
        self.hash ^= ZobristHasher::en_passant_hash(self.ep_target);
        self.ep_target = unretrievable_info.ep_target;
        self.hash ^= ZobristHasher::en_passant_hash(self.ep_target);
        self.hash ^= ZobristHasher::castling_rights_hash(self.castling_rights);
        self.castling_rights = unretrievable_info.castling_rights;
        self.hash ^= ZobristHasher::castling_rights_hash(self.castling_rights);
        self.reversible_moves = unretrievable_info.reversible_moves;
        self.ply -= 1;
        self.side_to_move = self.side_to_move.opposite();
        self.hash ^= ZobristHasher::ZOBRIST_KEYS[ZobristHasher::BLACK_TO_MOVE_INDEX];

        if let Some(moved) = self.remove_piece(move_played.target()) {
            self.add_piece(moved, move_played.origin())
        }
        if let Some(captured) = captured_piece {
            self.add_piece(captured, move_played.target())
        }

        match move_played.flags() {
            MoveFlags::EnPassant => {
                self.add_piece(
                    Piece { piece_type: PieceType::Pawn, color: self.side_to_move.opposite() },
                    if self.side_to_move == White { move_played.target()-8 } else { move_played.target()+8 }
                )
            },
            MoveFlags::Promotion(_) | MoveFlags::PromotionCapture(_) => {
                self.remove_piece(move_played.origin());
                self.add_piece(Piece { piece_type: Pawn, color: self.side_to_move }, move_played.origin());
            },
            MoveFlags::KingSideCastle => {
                let moved_rook = self.remove_piece(if self.side_to_move == White { 5 } else { 61 }).unwrap();
                self.add_piece(moved_rook, if self.side_to_move == White { 7 } else { 63 } )
            },
            MoveFlags::QueenSideCastle => {
                let moved_rook = self.remove_piece(if self.side_to_move == White { 3 } else { 59 }).unwrap();
                self.add_piece(moved_rook, if self.side_to_move == White { 0 } else { 56 } )
            },
            _ => ()
        }
    }

    fn unmake_null(&mut self, ep_target: Option<Square>) {
        self.hash ^= ZobristHasher::en_passant_hash(ep_target);
        self.ep_target = ep_target;
        self.hash ^= ZobristHasher::ZOBRIST_KEYS[ZobristHasher::BLACK_TO_MOVE_INDEX];
        self.side_to_move = self.side_to_move.opposite();
        self.ply -= 1;
    }

    /// Places a new piece on a given square
    pub fn add_piece(&mut self, piece: Piece, sq: usize) {
        self.pieces[sq] = Some(piece);
        self.bitboards[piece.color as usize][piece.piece_type as usize].set(sq);
        self.bitboards[piece.color as usize][6].set(sq);
        self.hash ^= ZobristHasher::hash_for_piece_sq(piece, sq);
    }

    /// Clears the given square, returning the piece that has been removed if any
    pub fn remove_piece(&mut self, sq: usize) -> Option<Piece> {
        let removed = self.pieces[sq];
        self.pieces[sq] = None;
        if let Some(p) = removed {
            self.bitboards[p.color as usize][p.piece_type as usize].unset(sq);
            self.bitboards[p.color as usize][6].unset(sq);
            self.hash ^= ZobristHasher::hash_for_piece_sq(p, sq);
        }
        removed
    }

    /*
    INTERESTING GETTERS
     */
    pub fn side_to_move(&self) -> Color { self.side_to_move }

    pub fn en_passant_target(&self) -> Option<Square> {
        self.ep_target
    }

    pub fn side_to_move_castling_rights(&self) -> (bool, bool) {
        self.castling_rights.get(self.side_to_move)
    }

    pub fn halfmove_clock(&self) -> u32 {
        self.ply
    }

    pub fn get_piece_bitboard(&self, piece_type: PieceType, color: Color) -> Bitboard {
        self.bitboards[color as usize][piece_type as usize]
    }

    pub fn get_piecetype_bitboard(&self, piece_type: PieceType) -> Bitboard {
        self.bitboards[0][piece_type as usize] | self.bitboards[1][piece_type as usize]
    }

    pub fn get_color_bitboard(&self, color: Color) -> Bitboard {
        self.bitboards[color as usize][6]
    }

    pub fn get_occupancy_bitboard(&self) -> Bitboard {
        self.bitboards[0][6] | self.bitboards[1][6]
    }

    pub fn get_diagonal_sliders_bitboard(&self, color: Color) -> Bitboard {
        self.bitboards[color as usize][2] | self.bitboards[color as usize][4]
    }
    pub fn get_cardinal_sliders_bitboard(&self, color: Color) -> Bitboard {
        self.bitboards[color as usize][3] | self.bitboards[color as usize][4]
    }

    pub fn king_square(&self, color: Color) -> Option<Square> {
        if self.bitboards[color as usize][5] == Bitboard::EMPTY {
            None
        } else {
            Some(self.bitboards[color as usize][5].ls1b())
        }
    }

    pub fn piece_type_on(&self, sq: Square) -> Option<PieceType> {
        self.pieces[sq].map(|p| p.piece_type)
    }

    pub fn color_on(&self, sq: Square) -> Option<Color> {
        self.pieces[sq].map(|p| p.color)
    }

    pub fn last_was_capture(&self) -> bool {
        self.history_entries[self.history_entries.len() - 1].move_played.is_capture()
    }

    pub fn in_check(&self, side: Color) -> bool {
        let king_square = if let Some(sq) = self.king_square(side) { sq } else { return true };
        self.attackers_of_square(king_square, side.opposite(), Bitboard::EMPTY) != Bitboard::EMPTY
    }

    pub fn fifty_move_draw(&self) -> bool { self.reversible_moves >= 50 }

    pub fn repetitions(&self, hash: Hash) -> usize {
        self.repeatable.iter().filter(|h| hash == **h).count()
    }

    pub fn is_drawn(&self) -> bool {
        self.repetitions(self.hash) >= 3 || self.fifty_move_draw()
    }

    /// Generates an attack map
    pub fn attack_map(&self, attacking_color: Color, ignore_king: bool) -> Bitboard {
        let mut attack_map = Bitboard::EMPTY;
        let occupancy = if ignore_king {
            self.get_occupancy_bitboard() & !self.bitboards[attacking_color.opposite() as usize][5]
        } else {
            self.get_occupancy_bitboard()
        };

        attack_map |= Bitboard::pawn_attacks(self.bitboards[attacking_color as usize][0], attacking_color);
        for sq in self.bitboards[attacking_color as usize][1] { attack_map |= Bitboard::KNIGHT_ATTACKS[sq] };
        for sq in self.bitboards[attacking_color as usize][5] { attack_map |= Bitboard::KING_ATTACKS[sq] };
        attack_map |= Bitboard::bishop_attacks_setwise(self.get_diagonal_sliders_bitboard(attacking_color), !occupancy);
        attack_map |= Bitboard::rook_attacks_setwise(self.get_cardinal_sliders_bitboard(attacking_color), !occupancy);

        attack_map
    }

    /// Returns a bitboard with attackers of a square
    pub fn attackers_of_square(&self, target: Square, attacking_color: Color, ignore_bb: Bitboard) -> Bitboard {
        let mut attackers = Bitboard::EMPTY;
        let occupancy = self.get_occupancy_bitboard() & !ignore_bb;
        attackers |= Bitboard::pawn_attacks(Bitboard::from_square(target), attacking_color.opposite()) & self.bitboards[attacking_color as usize][0];
        attackers |= Bitboard::KNIGHT_ATTACKS[target] & self.bitboards[attacking_color as usize][1];
        attackers |= Bitboard::bishop_attacks(target, occupancy) & self.get_diagonal_sliders_bitboard(attacking_color);
        attackers |= Bitboard::rook_attacks(target, occupancy) & self.get_cardinal_sliders_bitboard(attacking_color);
        attackers
    }

    /// Generates two distinct bitboards: (pinned pieces, pinning pieces).
    pub fn pins(&self, relative_to: Square) -> (Bitboard, Bitboard) {
        let mut pinners_bb = Bitboard::EMPTY;
        let mut pinned_bb = Bitboard::EMPTY;

        let occupancy = self.get_occupancy_bitboard();
        let friendly = self.get_color_bitboard(self.side_to_move);
        let attacking_side = self.side_to_move.opposite();

        let bishop_sliders = self.get_diagonal_sliders_bitboard(attacking_side);
        let bishop_pinners = Bitboard::xray_bishop_attacks(relative_to, occupancy, friendly) & bishop_sliders;
        for potential_pinner in bishop_pinners {
            let pinned_piece = Bitboard::get_ray(relative_to, potential_pinner) & friendly;
            pinned_bb |= pinned_piece;
            if pinned_piece != Bitboard::EMPTY { pinners_bb.set(potential_pinner) }
        }
        let rook_sliders = self.get_cardinal_sliders_bitboard(attacking_side);
        let rook_pinners = Bitboard::xray_rook_attacks(relative_to, occupancy, friendly) & rook_sliders;
        for potential_pinner in rook_pinners {
            let pinned_piece = Bitboard::get_ray(relative_to, potential_pinner) & friendly;
            pinned_bb |= pinned_piece;
            if pinned_piece != Bitboard::EMPTY { pinners_bb.set(potential_pinner) }
        }
        (pinned_bb, pinners_bb)
    }

    /// A simple iterator over material, each item being a piece type, and its associated bitboard
    pub fn material_iter(&self, color: Color) -> impl Iterator<Item = (PieceType, &Bitboard)> {
        self.bitboards[color as usize][0..6]
            .iter()
            .enumerate()
            .map(|(i, bb)| (PieceType::from_determinant(i).unwrap(), bb))
    }

    /*
    FEN STRING OPERATIONS
     */
    fn set_fen(&mut self, fen: &str) {
        let mut sections = fen.split(' ');
        let get = |x: Option<&str>| { match x {
            Some(s) => s.to_owned(),
            _ => "".to_owned()
        }};
        let piece_placement = get(sections.next());
        let side = get(sections.next());
        let castling = get(sections.next());
        let ep_target = get(sections.next());
        let halfmove = get(sections.next());
        let fullmove = get(sections.next());

        self.side_to_move = if side == "w" { White } else { Black };
        self.reversible_moves = halfmove.parse::<u32>().unwrap();
        self.ply = fullmove.parse::<u32>().unwrap();
        self.castling_rights = CastlingRights::from_str(&castling);
        self.ep_target = parse_square(&ep_target);

        let mut current_square: usize = 56;
        for c in piece_placement.chars() {
            if c == '/' { current_square -= 16 }
            else if c.is_digit(10) { current_square += c.to_digit(10).unwrap() as usize }
            else {
                self.add_piece(Piece::from_char(c).unwrap(), current_square);
                current_square += 1;
            }
        }

        self.history_entries = Vec::with_capacity(128);
        self.hash = self.cold_hash();
    }

    pub fn get_fen(&self) -> String {
        let mut fen = String::new();

        let mut current_square = 56;
        let mut empty_counter = 0;
        loop {
           match self.pieces[current_square] {
               Some(p) => {
                   if empty_counter != 0 { fen.push_str(&empty_counter.to_string()) }
                   empty_counter = 0;
                   fen.push_str(&p.to_string())
               }
               None => empty_counter += 1
           }

            current_square += 1;
            if current_square == 8 { break; }
            if current_square % 8 == 0 {
                if empty_counter != 0 { fen.push_str(&empty_counter.to_string()) }
                empty_counter = 0;
                fen.push('/');
                current_square -= 16;
            }
        }

        fen.push_str(if self.side_to_move == Color::White { " w " } else { " b " });
        fen.push_str(&self.castling_rights.to_string());
        match self.ep_target {
            Some(sq) => fen.push_str(&(" ".to_owned() + &square_representation(sq).unwrap() + " ")),
            None => fen.push_str(" - ")
        }
        fen.push_str(&(self.reversible_moves.to_string() + " "));
        fen.push_str(&self.ply.to_string());
        fen
    }

    /*
    HASHING
     */
    pub fn get_hash(&self) -> Hash { self.hash }

    fn cold_hash(&self) -> Hash {
        let mut hash = 0u64;
        for (sq, maybe_piece) in self.pieces.iter().enumerate() {
            if let Some(piece) = maybe_piece {
                hash ^= ZobristHasher::hash_for_piece_sq(*piece, sq)
            }
        } 
        hash ^= ZobristHasher::side_to_move_hash(self.side_to_move);
        hash ^= ZobristHasher::castling_rights_hash(self.castling_rights);
        hash ^= ZobristHasher::en_passant_hash(self.ep_target);
        hash
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut ranks: Vec<String> = vec!();
        let mut line: String = String::new();
        for sq in 0..64 {
            if sq%8 == 0 && sq != 0 {
                ranks.push(line);
                line = String::new()
            }
            match self.pieces[sq] {
                None => line.push_str(". "),
                Some(p) => line.push_str(&(p.to_string() + " "))
            }
        }
        ranks.push(line);
        ranks.reverse();

        let mut ranks_iter = ranks.iter();
        writeln!(f, "{}", ranks_iter.next().unwrap())?;
        writeln!(f, "{}  side to move: {}", ranks_iter.next().unwrap(), self.side_to_move)?;
        writeln!(f, "{}  castling_rights: {}", ranks_iter.next().unwrap(), self.castling_rights)?;
        writeln!(f, "{}  en passant: {}", ranks_iter.next().unwrap(), if let Some(sq) = self.ep_target {
            square_representation(sq).unwrap()
        } else {
            String::from("-")
        })?;
        writeln!(f, "{}  ply: {} ({} reversible moves)", ranks_iter.next().unwrap(), self.ply, self.reversible_moves)?;
        writeln!(f, "{}  hash: {:#0x}", ranks_iter.next().unwrap(), self.hash).unwrap();
        writeln!(f, "{}  fen: {}", ranks_iter.next().unwrap(), self.get_fen())?;
        write!(f, "{}", ranks_iter.next().unwrap())
    }
}