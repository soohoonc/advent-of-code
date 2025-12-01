(* @see: https://adventofcode.com/2025/day/1 *)

let parse input =
  let lines =
    input
    |> Core.String.split_on_chars ~on:[ '\n' ]
    |> List.filter (fun s -> not (Core.String.is_empty s))
  in
  List.map
    (fun line ->
      let direction = Core.String.sub line ~pos:0 ~len:1 in
      let length = Core.String.length line in
      let value = Core.String.sub line ~pos:1 ~len:(length - 1) in
      (direction, int_of_string value))
    lines

let solve_part_one input =
  let clean_input = parse input in
  snd
    (List.fold_left
       (fun (position, password) op ->
         let position' =
           (match fst op with
           | "L" -> position - snd op
           | "R" -> position + snd op
           | _ -> failwith "Invalid operation")
           mod 100
         in
         (position', password + Bool.to_int (position' = 0)))
       (50, 0) clean_input)

let solve_part_two input =
  let clean_input = parse input in
  snd
    (List.fold_left
       (fun (position, password) op ->
         let position' =
           match fst op with
           | "L" -> position - snd op
           | "R" -> position + snd op
           | _ -> failwith "Invalid operation"
         in
         let rotations =
           abs (position' / 100)
           + Bool.to_int (position' <= 0)
           - Bool.to_int
               (position' < 0 && position' mod 100 <> 0 && position = 0)
         in
         (((position' mod 100) + 100) mod 100, password + rotations))
       (50, 0) clean_input)

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
