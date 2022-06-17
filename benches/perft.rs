use chameleon::board::Board;
use chameleon::move_generator::{generate, GenType};
use criterion::{criterion_group, criterion_main, Criterion};

// A pure perft function, with no node counting, simply make/unmake and
// move generation
fn perft(board: &mut Board, depth: u64) {
    if depth == 0 {
        return;
    }
    for m in &generate(board, GenType::Legal) {
        board.make(*m);
        perft(board, depth - 1);
        board.unmake();
    }
}

fn perft_bench(c: &mut Criterion) {
    // The positions are taken from the chess programming wiki
    // https://www.chessprogramming.org/Perft_Results
    let mut board = Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    c.bench_function("perft initial 4", |b| b.iter(|| perft(&mut board, 4)));
    board = Board::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
    c.bench_function("perft kiwipete 4", |b| b.iter(|| perft(&mut board, 4)));
    board = Board::new("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
    c.bench_function("perft alternative 4", |b| b.iter(|| perft(&mut board, 4)));
}

criterion_group!(benches, perft_bench);
criterion_main!(benches);
