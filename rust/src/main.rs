use dotenv::dotenv;
use std::env;
use tokio;

mod utils;
#[path = "../2023/mod.rs"]
mod y2023;

#[tokio::main]
async fn main() {
    dotenv().ok();
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: cargo run <year> <day>");
        std::process::exit(1);
    }
    let year: u32 = args[1].parse().expect("Year must be a number");
    let day: u32 = args[2].parse().expect("Day must be a number");
    let input = utils::get_input(year, day).await;

    let result = match year {
        2023 => match day {
            1 => format!(
                "Part 1:\n{}\n\nPart 2:\n{}",
                y2023::day_01::solve_part_one(&input),
                y2023::day_01::solve_part_two(&input)
            ),
            1 => format!(
                "Part 1:\n{}\n\nPart 2:\n{}",
                y2023::day_03::solve_part_one(&input),
                y2023::day_03::solve_part_two(&input)

            )
            _ => panic!("Day {} not implemented for year {}", day, year),
        },
        _ => panic!("Year {} not implemented", year),
    };
    println!("{}", result);
}
