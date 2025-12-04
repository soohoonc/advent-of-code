(* @see: https://adventofcode.com/2025/day/3 *)

let parse input = input |> Core.String.split_lines
let char_to_int char = int_of_char char - 48

let find_max start stop str =
  let max = ref (char_to_int (String.get str start)) and max_idx = ref start in
  let rec loop = function
    | i when i = stop -> ()
    | i ->
        let value = char_to_int (String.get str i) in
        if value >= !max then (
          max_idx := i;
          max := value)
        else ();
        loop (i - 1)
  in
  loop start;
  (!max, !max_idx)

let solve_part_one input =
  List.fold_left
    (fun acc line ->
      let length = String.length line in
      let a, a_idx = find_max (length - 2) (-1) line in
      let b, _ = find_max (length - 1) a_idx line in
      acc + (a * 10) + b)
    0 (parse input)

let solve_part_two input =
  List.fold_left
    (fun acc line ->
      let length = String.length line and prev_idx = ref (-1) in
      let rec loop sum = function
        | i when i = 0 -> sum
        | i ->
            let max_idx = snd (find_max (length - i) !prev_idx line) in
            let valid_max_idx = min max_idx (length - i) in
            let curr = char_to_int (String.get line valid_max_idx) in
            prev_idx := valid_max_idx;
            loop ((sum * 10) + curr) (i - 1)
      in
      acc + loop 0 12)
    0 (parse input)

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
