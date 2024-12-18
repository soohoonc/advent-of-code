(* @see: https://adventofcode.com/2024/day/17 *)

module CharMap = Map.Make (Char)

let parse input =
  let registers = CharMap.empty in
  let extract (regs, program) s =
    if String.starts_with ~prefix:"Register" s then
      let reg, value = Scanf.sscanf s "Register %c: %d" (fun r v -> (r, v)) in
      (CharMap.add reg value regs, program)
    else if String.starts_with ~prefix:"Program: " s then
      let programs =
        Scanf.sscanf s "Program: %s" (fun nums -> nums)
        |> Core.String.split_on_chars ~on:[ ',' ]
        |> List.map int_of_string
      in
      (regs, programs :: program)
    else (regs, program)
  in
  input |> Core.String.split_lines
  |> List.filter (fun s -> String.empty <> s)
  |> List.fold_left extract (registers, [])

let solve_part_one _ = 0
let solve_part_two _ = 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
