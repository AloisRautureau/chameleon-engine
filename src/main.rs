fn main() {
    println!("chameleon v{}, by {}", env!("CARGO_PKG_VERSION"), env!("CARGO_PKG_AUTHORS"));

    let perft_mode = std::env::args().len() >= 2 && std::env::args().nth(1).unwrap() == "perft";

    // Launches a perft test
    if perft_mode {
        let expected_format = "Expected : perft <depth> [<FEN>]";
        let depth = std::env::args().nth(2).expect(expected_format).parse::<u32>().expect(expected_format);
        let fen = std::env::args().nth(3);

        chameleon::perft(depth, fen)
    } else { // Normal UCI
        chameleon::uci::UCI::run()
    }
}
