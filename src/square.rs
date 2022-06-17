pub type Square = usize;

pub fn rank_of(sq: Square) -> usize {
    sq / 8
}
pub fn file_of(sq: Square) -> usize {
    sq % 8
}
pub fn vertical_symmetry(sq: Square) -> Square {
    8 * (7 - rank_of(sq)) + file_of(sq)
}

/// Parses a square from a given string slice,
/// only caring that the first two characters form a valid square representation
/// ```
/// use chameleon::square::parse_square;
/// assert_eq!(parse_square("e4"), Some(28));
/// assert_eq!(parse_square("d2someotherstuff"), Some(11));
/// assert_eq!(parse_square("randoma1stuff"), None);
/// assert_eq!(parse_square("k9"), None);
/// ```
pub fn parse_square(s: &str) -> Option<Square> {
    let (rank, file);
    let mut chars_iter = s.chars();
    rank = match chars_iter.next() {
        Some(c) => match c {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _ => return None,
        },
        _ => return None,
    };
    file = match chars_iter.next() {
        Some(c) => match c.to_digit(10) {
            Some(i) if i <= 8 && i > 0 => i - 1,
            _ => return None,
        },
        _ => return None,
    };
    Some((file * 8 + rank) as usize)
}

/// Returns the string representation of a square
/// ```
/// use chameleon::square::square_representation;
/// assert_eq!(square_representation(28), Some(String::from("e4")));
/// assert_eq!(square_representation(11), Some(String::from("d2")));
/// assert_eq!(square_representation(65), None);
/// ```
pub fn square_representation(sq: Square) -> Option<String> {
    let rank = ('1'..='8').nth(rank_of(sq))?;
    let file = ('a'..='h').nth(file_of(sq))?;
    let mut repr = file.to_string();
    repr.push(rank);
    Some(repr)
}

/// Checks whether a given square is in bounds (0..64) or not
/// ```
/// use chameleon::square::is_valid;
/// assert!(is_valid(12));
/// ```
pub fn is_valid(sq: Square) -> bool {
    sq < 64
}
