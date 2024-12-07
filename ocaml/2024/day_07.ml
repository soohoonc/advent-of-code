(* @see: https://adventofcode.com/2024/day/7 *)

let parse input =
  input
  |> Core.String.split_on_chars ~on:[ '\n'; '\r' ]
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun line ->
         match Core.String.split_on_chars ~on:[ ':' ] line with
         | [ num; rest ] ->
             let num = int_of_string num in
             let nums =
               Core.String.split_on_chars ~on:[ ' ' ] (String.trim rest)
               |> List.filter (fun s -> String.trim s <> "")
               |> List.map int_of_string
             in
             (num, nums)
         | _ -> failwith "Invalid input format")

let solve_part_one input =
  let equations = parse input in
  let valid (value, nums) =
    match nums with
    | first :: rest ->
        let rec valid_nums acc = function
          | [] -> acc = value
          | h :: t when acc <= value ->
              valid_nums (acc + h) t || valid_nums (acc * h) t
          | _ -> false
        in
        valid_nums first rest
    | [] -> false
  in
  let rec check_equations acc = function
    | [] -> acc
    | h :: t -> check_equations (acc + if valid h then fst h else 0) t
  in
  check_equations 0 equations

let digits n =
  let rec count acc = function 0 -> acc | n -> count (acc + 1) (n / 10) in
  count 0 n

let solve_part_two input =
  let equations = parse input in
  let valid (value, nums) =
    match nums with
    | first :: rest ->
        let rec valid_nums acc = function
          | [] -> acc = value
          | h :: t when acc <= value ->
              valid_nums (acc + h) t
              || valid_nums ((acc * Base.Int.pow 10 (digits h)) + h) t
              || valid_nums (acc * h) t
          | _ -> false
        in
        valid_nums first rest
    | [] -> false
  in
  let rec check_equations acc = function
    | [] -> acc
    | h :: t -> check_equations (acc + if valid h then fst h else 0) t
  in
  check_equations 0 equations

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
