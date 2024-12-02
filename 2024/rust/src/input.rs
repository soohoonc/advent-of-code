pub async fn get_input(
    day: u8,
    session: String,
    year: Option<u16>,
) -> Result<String, Box<dyn std::error::Error>> {
    let url = format!(
        "https://adventofcode.com/{}/day/{}/input",
        year.unwrap_or(2024),
        day
    );
    let response = reqwest::Client::new()
        .get(url)
        .header("Cookie", format!("session={}", session))
        .send()
        .await?;
    let text = response.text().await?;
    Ok(text)
}
