(* @see: https://adventofcode.com/2024/day/4 *)

let parse input =
  input
  |> Core.String.split_on_chars ~on:[ '\n' ]
  |> List.filter (fun s -> not (Core.String.is_empty s))

let solve_part_one input =
  let lines = parse input in
  let word = "XMAS" in
  let directions =
    [ (0, 1); (1, 0); (0, -1); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1) ]
  in
  let in_bounds x y =
    x >= 0 && y >= 0
    && x < List.length lines
    && y < String.length (List.nth lines x)
  in
  let count_words x y pos =
    let rec loop_directions acc = function
      | [] -> acc
      | h :: t ->
          let rec validate_word x y pos direction =
            match (x, y, pos) with
            | x, y, z
              when in_bounds x y
                   && Core.String.nget (List.nth lines x) y = word.[z] ->
                if z < String.length word - 1 then
                  validate_word
                    (x - fst direction)
                    (y - snd direction)
                    (z + 1) direction
                else true
            | _ -> false
          in
          let valid_word = validate_word x y pos h in
          loop_directions (acc + if valid_word then 1 else 0) t
    in
    loop_directions 0 directions
  in
  let rec loop acc x y =
    match (x, y) with
    | x, _ when x >= List.length lines -> acc
    | _, y when y >= String.length (List.nth lines x) -> loop acc (x + 1) 0
    | x, y ->
        let valid_words = count_words x y 0 in
        loop (acc + valid_words) x (y + 1)
  in
  loop 0 0 0

let solve_part_two input =
  let lines = parse input in
  let in_bounds x y =
    x >= 0 && y >= 0
    && x < List.length lines
    && y < String.length (List.nth lines x)
  in
  let validate_x x y =
    Core.String.nget (List.nth lines x) y = 'A'
    && in_bounds (x + 1) (y + 1)
    && in_bounds (x - 1) (y - 1)
    && in_bounds (x - 1) (y + 1)
    && in_bounds (x - 1) (y + 1)
    && (let up = Core.String.nget (List.nth lines (x + 1)) (y + 1)
        and down = Core.String.nget (List.nth lines (x - 1)) (y - 1) in
        (up = 'M' && down = 'S') || (up = 'S' && down = 'M'))
    &&
    let up = Core.String.nget (List.nth lines (x - 1)) (y + 1)
    and down = Core.String.nget (List.nth lines (x + 1)) (y - 1) in
    (up = 'M' && down = 'S') || (up = 'S' && down = 'M')
  in
  let rec loop acc x y =
    match (x, y) with
    | x, _ when x >= List.length lines -> acc
    | _, y when y >= String.length (List.nth lines x) -> loop acc (x + 1) 0
    | x, y ->
        let valid_x = validate_x x y in
        loop (acc + if valid_x then 1 else 0) x (y + 1)
  in
  loop 0 0 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
