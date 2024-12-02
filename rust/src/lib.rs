use dotenv::dotenv;
use reqwest::Client;
use std::env;

pub async fn get_input(year: u32, day: u32) -> String {
    dotenv().ok();
    let session = env::var("AOC_SESSION").expect("AOC_SESSION must be defined!");
    let url = format!("https://adventofcode.com/{}/day/{}/input", year, day);

    Client::new()
        .get(&url)
        .header("Cookie", format!("session={}", session))
        .send()
        .await
        .expect("Failed to send request")
        .text()
        .await
        .expect("Failed to get response text")
}
