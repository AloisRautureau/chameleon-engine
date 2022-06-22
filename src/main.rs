use chameleon::uci::UCI;

fn main() {
    println!(
        "chameleon v{}, by {}",
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_AUTHORS")
    );

    let mut uci = UCI::default();
    uci.run()
}
