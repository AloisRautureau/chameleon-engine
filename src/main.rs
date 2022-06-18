fn main() {
    println!(
        "chameleon v{}, by {}",
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_AUTHORS")
    );

    chameleon::uci::UCI::run()
}
