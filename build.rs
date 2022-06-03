use std::env;
use std::fs::File;
use std::path::Path;
use std::io::Write;
use rand_mt::Mt64;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let generation_file = Path::new(&out_dir).join("lookup.rs");
    let evaluation_file = Path::new(&out_dir).join("evaluation_constants.rs");
    let zobrist_file = Path::new(&out_dir).join("zobrist_keys.rs");
    let mut file = File::create(generation_file).unwrap();

    println!("aled");

    let processed_consts = BuildPreprocessor::process();
    println!("finished processing");
    writeln!(&mut file, "impl Bitboard {{").unwrap();

    inject_array(
        &mut file,
        "pub const KNIGHT_ATTACKS: [Bitboard; 64]",
        &processed_consts.knight_attacks, 
        Some("Bitboard")
    );
    inject_array(
        &mut file,
        "pub const KING_ATTACKS: [Bitboard; 64]",
        &processed_consts.king_attacks, 
        Some("Bitboard")
    );
    inject_array(
        &mut file,
        "pub const BISHOP_MAGICS: [u64; 64]",
        &processed_consts.bishop_magics, 
        None
    );
    inject_array(
        &mut file,
        "pub const BISHOP_MAGIC_SHIFTS: [usize; 64]",
        &processed_consts.bishop_magic_bits, 
        None
    );
    inject_array(
        &mut file,
        "pub const BISHOP_MASKS: [Bitboard; 64]",
        &processed_consts.bishop_masks,
        Some("Bitboard")
    );
    inject_array(
        &mut file,
        "pub const ROOK_MAGICS: [u64; 64]",
        &processed_consts.rook_magics, 
        None
    );
    inject_array(
        &mut file,
        "pub const ROOK_MAGIC_SHIFTS: [usize; 64]",
        &processed_consts.rook_magic_bits, 
        None
    );
    inject_array(
        &mut file,
        "pub const ROOK_MASKS: [Bitboard; 64]",
        &processed_consts.rook_masks,
        Some("Bitboard")
    );


    inject_2d_array(
        &mut file,
        "pub const RAYS: [[Bitboard; 64]; 8]",
        Vec::from(processed_consts.rays.map(Vec::from)),
        Some("Bitboard")
    );
    inject_2d_array(
        &mut file,
        "pub const ORIGIN_TARGET_RAYS: [[Bitboard; 64]; 64]",
        Vec::from(processed_consts.origin_target_rays.map(Vec::from)),
        Some("Bitboard")
    );
    inject_2d_array(
        &mut file,
        "pub const BISHOP_ATTACKS_TABLE: [[Bitboard; 1024]; 64]",
        processed_consts.bishop_attacks,
        Some("Bitboard")
    );
    inject_2d_array(
        &mut file,
        "pub const ROOK_ATTACKS_TABLE: [[Bitboard; 4096]; 64]",
        processed_consts.rook_attacks,
        Some("Bitboard")
    );
    writeln!(&mut file, "}}").unwrap();

    file = File::create(evaluation_file).unwrap();
    writeln!(
        &mut file,
        "
        // Piece-square tables taken from PeSTO's evaluation function
        // https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function
        pub const MIDGAME_PIECE_SQUARE_TABLE: [[Score; 64]; 6] = [
            [
                0,   0,   0,   0,   0,   0,  0,   0,
                98, 134,  61,  95,  68, 126, 34, -11,
                -6,   7,  26,  31,  65,  56, 25, -20,
               -14,  13,   6,  21,  23,  12, 17, -23,
               -27,  -2,  -5,  12,  17,   6, 10, -25,
               -26,  -4,  -4, -10,   3,   3, 33, -12,
               -35,  -1, -20, -23, -15,  24, 38, -22,
                 0,   0,   0,   0,   0,   0,  0,   0,
            ],
            [
                -167, -89, -34, -49,  61, -97, -15, -107,
                -73, -41,  72,  36,  23,  62,   7,  -17,
                -47,  60,  37,  65,  84, 129,  73,   44,
                 -9,  17,  19,  53,  37,  69,  18,   22,
                -13,   4,  16,  13,  28,  19,  21,   -8,
                -23,  -9,  12,  10,  19,  17,  25,  -16,
                -29, -53, -12,  -3,  -1,  18, -14,  -19,
               -105, -21, -58, -33, -17, -28, -19,  -23,
            ],
            [
                -29,   4, -82, -37, -25, -42,   7,  -8,
                -26,  16, -18, -13,  30,  59,  18, -47,
                -16,  37,  43,  40,  35,  50,  37,  -2,
                 -4,   5,  19,  50,  37,  37,   7,  -2,
                 -6,  13,  13,  26,  34,  12,  10,   4,
                  0,  15,  15,  15,  14,  27,  18,  10,
                  4,  15,  16,   0,   7,  21,  33,   1,
                -33,  -3, -14, -21, -13, -12, -39, -21,
            ],
            [
                32,  42,  32,  51, 63,  9,  31,  43,
                27,  32,  58,  62, 80, 67,  26,  44,
                -5,  19,  26,  36, 17, 45,  61,  16,
               -24, -11,   7,  26, 24, 35,  -8, -20,
               -36, -26, -12,  -1,  9, -7,   6, -23,
               -45, -25, -16, -17,  3,  0,  -5, -33,
               -44, -16, -20,  -9, -1, 11,  -6, -71,
               -19, -13,   1,  17, 16,  7, -37, -26,
            ],
            [
                -28,   0,  29,  12,  59,  44,  43,  45,
                -24, -39,  -5,   1, -16,  57,  28,  54,
                -13, -17,   7,   8,  29,  56,  47,  57,
                -27, -27, -16, -16,  -1,  17,  -2,   1,
                 -9, -26,  -9, -10,  -2,  -4,   3,  -3,
                -14,   2, -11,  -2,  -5,   2,  14,   5,
                -35,  -8,  11,   2,   8,  15,  -3,   1,
                 -1, -18,  -9,  10, -15, -25, -31, -50,
            ],
            [
                -65,  23,  16, -15, -56, -34,   2,  13,
                29,  -1, -20,  -7,  -8,  -4, -38, -29,
                -9,  24,   2, -16, -20,   6,  22, -22,
               -17, -20, -12, -27, -30, -25, -14, -36,
               -49,  -1, -27, -39, -46, -44, -33, -51,
               -14, -14, -22, -46, -44, -30, -15, -27,
                 1,   7,  -8, -64, -43, -16,   9,   8,
               -15,  36,  12, -54,   8, -28,  24,  14,
            ],
        ];
        pub const ENDGAME_PIECE_SQUARE_TABLE: [[Score; 64]; 6] = [
            [
                0,   0,   0,   0,   0,   0,   0,   0,
                178, 173, 158, 134, 147, 132, 165, 187,
                 94, 100,  85,  67,  56,  53,  82,  84,
                 32,  24,  13,   5,  -2,   4,  17,  17,
                 13,   9,  -3,  -7,  -7,  -8,   3,  -1,
                  4,   7,  -6,   1,   0,  -5,  -1,  -8,
                 13,   8,   8,  10,  13,   0,   2,  -7,
                  0,   0,   0,   0,   0,   0,   0,   0,
            ],
            [
                -58, -38, -13, -28, -31, -27, -63, -99,
                -25,  -8, -25,  -2,  -9, -25, -24, -52,
                -24, -20,  10,   9,  -1,  -9, -19, -41,
                -17,   3,  22,  22,  22,  11,   8, -18,
                -18,  -6,  16,  25,  16,  17,   4, -18,
                -23,  -3,  -1,  15,  10,  -3, -20, -22,
                -42, -20, -10,  -5,  -2, -20, -23, -44,
                -29, -51, -23, -15, -22, -18, -50, -64,
            ],
            [
                -14, -21, -11,  -8, -7,  -9, -17, -24,
                -8,  -4,   7, -12, -3, -13,  -4, -14,
                 2,  -8,   0,  -1, -2,   6,   0,   4,
                -3,   9,  12,   9, 14,  10,   3,   2,
                -6,   3,  13,  19,  7,  10,  -3,  -9,
               -12,  -3,   8,  10, 13,   3,  -7, -15,
               -14, -18,  -7,  -1,  4,  -9, -15, -27,
               -23,  -9, -23,  -5, -9, -16,  -5, -17,
            ],
            [
                13, 10, 18, 15, 12,  12,   8,   5,
                11, 13, 13, 11, -3,   3,   8,   3,
                 7,  7,  7,  5,  4,  -3,  -5,  -3,
                 4,  3, 13,  1,  2,   1,  -1,   2,
                 3,  5,  8,  4, -5,  -6,  -8, -11,
                -4,  0, -5, -1, -7, -12,  -8, -16,
                -6, -6,  0,  2, -9,  -9, -11,  -3,
                -9,  2,  3, -1, -5, -13,   4, -20,
            ],
            [
                -9,  22,  22,  27,  27,  19,  10,  20,
                -17,  20,  32,  41,  58,  25,  30,   0,
                -20,   6,   9,  49,  47,  35,  19,   9,
                  3,  22,  24,  45,  57,  40,  57,  36,
                -18,  28,  19,  47,  31,  34,  39,  23,
                -16, -27,  15,   6,   9,  17,  10,   5,
                -22, -23, -30, -16, -16, -23, -36, -32,
                -33, -28, -22, -43,  -5, -32, -20, -41,
            ],
            [
                -74, -35, -18, -18, -11,  15,   4, -17,
                -12,  17,  14,  17,  17,  38,  23,  11,
                 10,  17,  23,  15,  20,  45,  44,  13,
                 -8,  22,  24,  27,  26,  33,  26,   3,
                -18,  -4,  21,  24,  27,  23,   9, -11,
                -19,  -3,  11,  21,  23,  16,   7,  -9,
                -27, -11,   4,  13,  14,   4,  -5, -17,
                -53, -34, -21, -11, -28, -14, -24, -43
            ],
        ];
        "
    ).unwrap();

    file = File::create(zobrist_file).unwrap();
    writeln!(&mut file, "impl ZobristHasher {{").unwrap();
    inject_array(
        &mut file,
        "pub const ZOBRIST_KEYS: [u64; 781]",
        &BuildPreprocessor::initialize_zobrist_keys(),
        None
    );
    writeln!(&mut file, "}}").unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}

fn inject_array<T: ToString>(file: &mut File, declaration: &str, array: &[T], constructor: Option<&str>) {
    println!("in function array_to_code with decl {}", declaration);

    writeln!(file, "{} = [", declaration).unwrap();
    for value in array {
        write!(file, "{}, ", match constructor {
            Some(c) => c.to_owned() + "(" + &value.to_string() + ")",
            None => value.to_string()
        }).unwrap();
    }
    writeln!(file, "];").unwrap();
}

fn inject_2d_array<T: ToString>(file: &mut File, declaration: &str, array: Vec<Vec<T>>, constructor: Option<&str>) {
    println!("in function array_to_code with decl {}", declaration);

    writeln!(file, "{} = [", declaration).unwrap();
    for slice in array {
        write!(file, "[").unwrap();
        for value in slice {
            write!(file, "{}, ", match constructor {
                Some(c) => c.to_owned() + "(" + &value.to_string() + ")",
                None => value.to_string()
            }).unwrap();
        }
        writeln!(file, "], ").unwrap();
    }
    writeln!(file, "];").unwrap();
}

struct BuildPreprocessor {
    pub knight_attacks: Vec<u64>,
    pub king_attacks: Vec<u64>,

    pub rays: [[u64; 64]; 8],
    pub origin_target_rays: [[u64; 64]; 64],

    pub bishop_magics: [u64; 64],
    pub bishop_magic_bits: [usize; 64],
    pub bishop_masks: [u64; 64],
    pub bishop_attacks: Vec<Vec<u64>>,
    pub rook_magics: [u64; 64],
    pub rook_magic_bits: [usize; 64],
    pub rook_masks: [u64; 64],
    pub rook_attacks: Vec<Vec<u64>>
}

impl BuildPreprocessor {
    pub fn process() -> BuildPreprocessor {
        let (bishop_magics, bishop_magic_bits) = Self::process_bishop_magics();
        let (rook_magics, rook_magic_bits) = Self::process_rook_magics();
        let rays = Self::process_rays();
        let bishop_masks = Self::process_bishop_masks(rays);
        let rook_masks = Self::process_rook_masks(rays);
        BuildPreprocessor {
            knight_attacks: Self::process_knight_moves(),
            king_attacks: Self::process_king_moves(),

            rays,
            origin_target_rays: Self::process_origin_target_rays(rays),

            bishop_magics,
            bishop_magic_bits,
            bishop_masks,
            bishop_attacks: Self::process_bishop_attacks(bishop_magics, bishop_magic_bits, rays, bishop_masks),
            rook_magics,
            rook_magic_bits,
            rook_masks,
            rook_attacks: Self::process_rook_attacks(rook_magics, rook_magic_bits, rays, rook_masks),
        }
    }

    fn process_knight_moves() -> Vec<u64> {
        println!("processing knight");
        let knight_shifts: [fn(u64) -> u64; 8] = [
            |bb| (bb << 10) & !0x303030303030303,
            |bb| (bb >> 10) & !0xc0c0c0c0c0c0c0c0,
            |bb| (bb << 17) & !0x101010101010101,
            |bb| (bb >> 17) & !0x8080808080808080,
            |bb| (bb << 15) & !0x8080808080808080,
            |bb| (bb >> 15) & !0x101010101010101,
            |bb| (bb << 6) & !0xc0c0c0c0c0c0c0c0,
            |bb| (bb >> 6) & !0x303030303030303,
        ];
        let mut results = vec!();
        for sq in 0..64 {
            let origin = 1u64 << sq;
            let mut attacks = 0u64;
            for shift in knight_shifts {
                attacks |= shift(origin);
            }
            results.push(attacks);
        }
        results
    }

    fn process_king_moves() -> Vec<u64> {
        println!("processing king");
        let king_shifts: [fn(u64) -> u64; 8] = [
            |bb| (bb >> 1) & !0x8080808080808080,
            |bb| (bb << 1) & !0x101010101010101,
            |bb| (bb << 7) & !0x8080808080808080,
            |bb| (bb >> 7) & !0x101010101010101,
            |bb| (bb << 9) & !0x101010101010101,
            |bb| (bb >> 9) & !0x8080808080808080,
            |bb| bb << 8,
            |bb| bb >> 8,
        ];
        let mut results = vec!();
        for sq in 0..64 {
            let origin = 1u64 << sq;
            let mut attacks = 0u64;
            for shift in king_shifts {
                attacks |= shift(origin);
            }
            results.push(attacks);
        }
        results
    }


    // TODO: Change this to calculate magics at compile time
    // Right now this uses magics from Shallow Blue by GunshipPenguin 
    // (https://github.com/GunshipPenguin/shallow-blue/blob/c6d7e9615514a86533a9e0ffddfc96e058fc9cfd/src/attacks.h#L120)
    fn process_rook_magics() -> ([u64; 64], [usize; 64]) {
        ([
            0xa8002c000108020, 0x6c00049b0002001, 0x100200010090040, 0x2480041000800801, 0x280028004000800,
            0x900410008040022, 0x280020001001080, 0x2880002041000080, 0xa000800080400034, 0x4808020004000,
            0x2290802004801000, 0x411000d00100020, 0x402800800040080, 0xb000401004208, 0x2409000100040200,
            0x1002100004082, 0x22878001e24000, 0x1090810021004010, 0x801030040200012, 0x500808008001000,
            0xa08018014000880, 0x8000808004000200, 0x201008080010200, 0x801020000441091, 0x800080204005,
            0x1040200040100048, 0x120200402082, 0xd14880480100080, 0x12040280080080, 0x100040080020080,
            0x9020010080800200, 0x813241200148449, 0x491604001800080, 0x100401000402001, 0x4820010021001040,
            0x400402202000812, 0x209009005000802, 0x810800601800400, 0x4301083214000150, 0x204026458e001401,
            0x40204000808000, 0x8001008040010020, 0x8410820820420010, 0x1003001000090020, 0x804040008008080,
            0x12000810020004, 0x1000100200040208, 0x430000a044020001, 0x280009023410300, 0xe0100040002240,
            0x200100401700, 0x2244100408008080, 0x8000400801980, 0x2000810040200, 0x8010100228810400,
            0x2000009044210200, 0x4080008040102101, 0x40002080411d01, 0x2005524060000901, 0x502001008400422,
            0x489a000810200402, 0x1004400080a13, 0x4000011008020084, 0x26002114058042
        ],[
            12, 11, 11, 11, 11, 11, 11, 12,
            11, 10, 10, 10, 10, 10, 10, 11,
            11, 10, 10, 10, 10, 10, 10, 11,
            11, 10, 10, 10, 10, 10, 10, 11,
            11, 10, 10, 10, 10, 10, 10, 11,
            11, 10, 10, 10, 10, 10, 10, 11,
            11, 10, 10, 10, 10, 10, 10, 11,
            12, 11, 11, 11, 11, 11, 11, 12
        ])
    }

    fn process_bishop_magics() -> ([u64; 64], [usize; 64]) {
        ([
            0x89a1121896040240, 0x2004844802002010, 0x2068080051921000, 0x62880a0220200808, 0x4042004000000,
            0x100822020200011, 0xc00444222012000a, 0x28808801216001, 0x400492088408100, 0x201c401040c0084,
            0x840800910a0010, 0x82080240060, 0x2000840504006000, 0x30010c4108405004, 0x1008005410080802,
            0x8144042209100900, 0x208081020014400, 0x4800201208ca00, 0xf18140408012008, 0x1004002802102001,
            0x841000820080811, 0x40200200a42008, 0x800054042000, 0x88010400410c9000, 0x520040470104290,
            0x1004040051500081, 0x2002081833080021, 0x400c00c010142, 0x941408200c002000, 0x658810000806011,
            0x188071040440a00, 0x4800404002011c00, 0x104442040404200, 0x511080202091021, 0x4022401120400,
            0x80c0040400080120, 0x8040010040820802, 0x480810700020090, 0x102008e00040242, 0x809005202050100,
            0x8002024220104080, 0x431008804142000, 0x19001802081400, 0x200014208040080, 0x3308082008200100,
            0x41010500040c020, 0x4012020c04210308, 0x208220a202004080, 0x111040120082000, 0x6803040141280a00,
            0x2101004202410000, 0x8200000041108022, 0x21082088000, 0x2410204010040, 0x40100400809000,
            0x822088220820214, 0x40808090012004, 0x910224040218c9, 0x402814422015008, 0x90014004842410,
            0x1000042304105, 0x10008830412a00, 0x2520081090008908, 0x40102000a0a60140,
        ],[
            6, 5, 5, 5, 5, 5, 5, 6,
            5, 5, 5, 5, 5, 5, 5, 5,
            5, 5, 7, 7, 7, 7, 5, 5,
            5, 5, 7, 9, 9, 7, 5, 5,
            5, 5, 7, 9, 9, 7, 5, 5,
            5, 5, 7, 7, 7, 7, 5, 5,
            5, 5, 5, 5, 5, 5, 5, 5,
            6, 5, 5, 5, 5, 5, 5, 6
        ])
    }

    fn process_rays() -> [[u64; 64]; 8] {
        println!("processing rays");
        let shifts: [fn(u64) -> u64; 8] = [
            |b| (b << 1) & !0x101010101010101, 
            |b| (b >> 1) & !0x8080808080808080, 
            |b| b << 8,
            |b| b >> 8, 
            |b| (b << 7) & !0x8080808080808080, 
            |b| (b >> 7) & !0x101010101010101,
            |b| (b << 9) & !0x101010101010101, 
            |b| (b >> 9) & !0x8080808080808080
        ];

        let mut results = [[0u64; 64]; 8];
        for sq in 0..64 {
            let origin = 1u64 << sq;
            for (i, shift) in shifts.iter().enumerate() {
                let mut ray = 0u64;
                let mut current = origin;
                while current != 0 {
                    current = shift(current);
                    ray |= current;
                }
                results[i][sq] = ray;
            }
        }
        results
    }

    pub fn process_origin_target_rays(rays: [[u64; 64]; 8]) -> [[u64; 64]; 64] {
        let mut origin_target_rays = [[0u64; 64]; 64];

        for (origin, origin_target_ray) in origin_target_rays.iter_mut().enumerate() {
            for target in 0..64 {
                for ray in &rays {
                    if ray[origin] & (1 << target) != 0u64 {
                        origin_target_ray[target] = ray[origin] & !ray[target];
                        break
                    }
                }
            }
        }

        origin_target_rays
    }

    fn blocker_by_index(index: usize, attack_mask: u64) -> u64 {
        let mut blockers = 0u64;
        let mut mask = attack_mask;
        for i in 0..attack_mask.count_ones() {
            let lsb = mask.trailing_zeros();
            if index & (1 << i) != 0 { blockers |= 1 << lsb }
            mask &= mask - 1
        }
        blockers
    }

    fn process_bishop_masks(rays: [[u64; 64]; 8]) -> [u64; 64] {
        let not_useful: [u64; 8] = [
            0x8080808080808080,
            0x101010101010101,
            0xff00000000000000,
            0xff,
            0x101010101010101 | 0xff00000000000000,
            0x8080808080808080 | 0xff,
            0x8080808080808080 | 0xff00000000000000,
            0x101010101010101 | 0xff
        ];
        let mut result = [0u64; 64];
        for (sq, mask) in result.iter_mut().enumerate() {
            for ray_index in 4..8 {
                *mask |= rays[ray_index][sq] & !not_useful[ray_index];
            }
        }
        result
    }

    fn process_bishop_attacks(magics: [u64; 64], shifts: [usize; 64], rays: [[u64; 64]; 8], masks: [u64; 64]) -> Vec<Vec<u64>> {
        println!("processing bishop");
        let mut bishop_attacks = vec!();
        for sq in 0..64 {
            let mut square_attacks = Vec::from([0u64; 1024]);
            for index in 0..(1 << shifts[sq]) {
                let blockers = Self::blocker_by_index(index, masks[sq]);
                let key = (blockers.wrapping_mul(magics[sq])) >> (64 - shifts[sq]);
                square_attacks[key as usize] = Self::bishop_attacks_slow(sq, blockers, rays)
            }
            bishop_attacks.push(square_attacks)
        }
        bishop_attacks
    }

    fn bishop_attacks_slow(origin: usize, blockers: u64, rays: [[u64; 64]; 8]) -> u64 {
        let mut attacks = 0u64;
        for (ray_index, rays_from) in rays.iter().enumerate().skip(4) {
            let ray = rays_from[origin];
            let blocked_by = blockers & ray;
            if blocked_by != 0u64 {
                let first_blocker_index = if ray_index % 2 == 0 { 
                    blocked_by.trailing_zeros() 
                } else { 
                    63 - blocked_by.leading_zeros() 
                };
                attacks |= ray & !rays_from[first_blocker_index as usize];
            }
            else {
                attacks |= ray
            }
        }
        attacks
    }

    fn process_rook_masks(rays: [[u64; 64]; 8]) -> [u64; 64] {
        let not_useful: [u64; 8] = [
            0x8080808080808080,
            0x101010101010101,
            0xff00000000000000,
            0xff,
            0x101010101010101 | 0xff00000000000000,
            0x8080808080808080 | 0xff,
            0x8080808080808080 | 0xff00000000000000,
            0x101010101010101 | 0xff
        ];
        let mut result = [0u64; 64];
        for (sq, mask) in result.iter_mut().enumerate() {
            for ray_index in 0..4 {
                *mask |= rays[ray_index][sq] & !not_useful[ray_index];
            }
        }
        result
    }

    fn process_rook_attacks(magics: [u64; 64], shifts: [usize; 64], rays: [[u64; 64]; 8], masks: [u64; 64]) -> Vec<Vec<u64>> {
        println!("processing rook");
        let mut rook_attacks = vec!();
        for sq in 0..64 {
            let mut square_attacks = Vec::from([0u64; 4096]);
            for index in 0..(1 << shifts[sq]) {
                let blockers = Self::blocker_by_index(index, masks[sq]);
                let key = (blockers.wrapping_mul(magics[sq])) >> (64 - shifts[sq]);
                square_attacks[key as usize] = Self::rook_attacks_slow(sq, blockers, rays)
            }
            rook_attacks.push(square_attacks)
        }
        rook_attacks
    }

    fn rook_attacks_slow(origin: usize, blockers: u64, rays: [[u64; 64]; 8]) -> u64 {
        let mut attacks = 0u64;
        for (ray_index, rays_from) in rays.iter().enumerate().take(4) {
            let ray = rays_from[origin];
            let blocked_by = blockers & ray;
            if blocked_by != 0u64 {
                let first_blocker_index = if ray_index % 2 == 0 { 
                    blocked_by.trailing_zeros() 
                } else { 
                    63 - blocked_by.leading_zeros() 
                };
                attacks |= ray & !rays_from[first_blocker_index as usize];
            }
            else {
                attacks |= ray
            }
        }
        attacks
    }

    pub fn initialize_zobrist_keys() -> [u64; 781] {
        let mut rng = Mt64::new_unseeded();
        [0u64; 781].map(|_| rng.next_u64())
    }
}