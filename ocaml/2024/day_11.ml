(* @see: https://adventofcode.com/2024/day/11 *)

let parse input =
  input
  |> Core.String.split_on_chars ~on:[ '\n'; ' ' ]
  |> List.filter (fun s -> not (Core.String.is_empty s))

let solve_part_one input =
  let stones = parse input in
  let apply_rules stone =
    if int_of_string stone = 0 then "1"
    else if Core.String.length stone mod 2 = 0 then
      let f =
        int_of_string
          (Core.String.sub stone ~pos:0 ~len:(Core.String.length stone / 2))
      in
      let s =
        int_of_string
          (Core.String.sub stone
             ~pos:(Core.String.length stone / 2)
             ~len:(Core.String.length stone / 2))
      in
      string_of_int f ^ " " ^ string_of_int s
    else string_of_int (int_of_string stone * 2024)
  in
  let rec loop stones = function
    | 0 -> stones
    | n ->
        loop
          (stones |> List.map apply_rules |> String.concat " "
          |> Core.String.split_on_chars ~on:[ ' ' ])
          (n - 1)
  in
  List.length (loop stones 25)

let solve_part_two input =
  let stones = parse input in
  let apply_rules stone =
    if int_of_string stone = 0 then "1"
    else if Core.String.length stone mod 2 = 0 then
      let f =
        int_of_string
          (Core.String.sub stone ~pos:0 ~len:(Core.String.length stone / 2))
      in
      let s =
        int_of_string
          (Core.String.sub stone
             ~pos:(Core.String.length stone / 2)
             ~len:(Core.String.length stone / 2))
      in
      string_of_int f ^ " " ^ string_of_int s
    else string_of_int (int_of_string stone * 2024)
  in
  let rec loop stones = function
    | 0 -> stones
    | n ->
        loop
          (stones |> List.map apply_rules |> String.concat " "
          |> Core.String.split_on_chars ~on:[ ' ' ])
          (n - 1)
  in
  List.length (loop stones 15)

(* let solve input = *)
let solve input =
  (* let input = "125 17" in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
