use dotenv::dotenv;
use std::env;

mod day_one;
mod input;

use crate::day_one::solve_part_one;
use crate::input::get_input;

async fn solve() {
    let session = env::var("AOC_SESSION").expect("AOC_SESSION must be defined!");
    let input = get_input(1, session.to_owned(), Some(2024));
    let result = solve_part_one(input);
}

fn main() {
    dotenv().ok();
    solve()
}
