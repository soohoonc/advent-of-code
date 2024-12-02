use aoc::get_input;

fn solve_part_one(input: String) {
    println!("{}", &input[..10]);
}

async fn solve() {
    let session = env::var("AOC_SESSION").expect("AOC_SESSION must be defined!");
    let input = get_input(1, session.to_owned(), Some(2024));
    let result = solve_part_one(input);
}

fn main() {
    dotenv().ok();
    solve()
}
