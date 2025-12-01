fn parse(input: &str) -> Vec<Vec<char>> {
    input.lines().map(|line| line.chars().collect()).collect()
}

pub fn solve_part_one(input: &str) -> String {
    let engine = parse(input);
    let mut result = 0;
    for (i, row) in engine.iter().enumerate() {
        for (j, &ch) in row.iter().enumerate() {
            if ch.is_numeric() {}
        }
    }
    result.to_string()
}

pub fn solve_part_two(input: &str) -> String {
    format!("Not implemented\nInput: {}...", input[..10].to_string())
}
