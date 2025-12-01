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
      (regs, programs)
    else (regs, program)
  in
  input |> Core.String.split_lines
  |> List.filter (fun s -> String.empty <> s)
  |> List.fold_left extract (registers, [])

(*
Part 1:
- 3-bit computer 
- registers A, B, C holding any int
- program: [opcode1, operand1, opcode2, operand2, ...] (3-bit)
- instruction pointer: that starts at 0, increase by 2 each time except for the jump instruction
- when instruction pointer tries to read out of bounds -> halts and returns output
- 2 types of operands:
  - literal: operand as value
  - combo: 0 - 3 -> literal | 4 - 6 -> val of registers A - C | 7 -> reserved
- 8 opcodes
  - 0 -> division register A / (2^(combo op)), truncated to int written to register A
  - 1 -> bitwise XOR of register B and instruction's literal operand written to register B
  - 2 -> combo operand mod 8 written to B
  - 3 -> no op if A is 0 else jumps instruction pointer to literal operand
  - 4 -> bitwise XOR of register B and C and written to register B (reads operand but ignore)
  - 5 -> calculates value of combo operand mod 8 and outputs that value. (if multiple outputs, return as comma separated list)
  - 6 -> like opcode 0 but written to B
  - 7 -> like opcode 0 but written to C

Part 2: minimum value in register A to output the same program

*)

let rec run ip registers program (output : int list) =
  (* ( let a, b, c = (CharMap.find 'A' registers), (CharMap.find 'B' registers), (CharMap.find 'C' registers) in
     Printf.printf "Running Instruction: %d, Registers A:%d, B:%d, C:%d\n" ip a b c); *)
  let combo v =
    match v with
    | 0 | 1 | 2 | 3 -> v
    | 4 -> CharMap.find 'A' registers
    | 5 -> CharMap.find 'B' registers
    | 6 -> CharMap.find 'C' registers
    | _ -> failwith "Invalid Combo"
  in
  if ip > List.length program - 1 then output
  else
    let opcode = List.nth program ip in
    let operand = List.nth program (ip + 1) in
    match opcode with
    | 0 ->
        let new_registers =
          let a = CharMap.find 'A' registers in
          (CharMap.add 'A' (a / Base.Int.pow 2 (combo operand))) registers
        in
        run (ip + 2) new_registers program output
    | 1 ->
        let new_registers =
          let b = CharMap.find 'B' registers in
          (CharMap.add 'B' (b lxor operand)) registers
        in
        run (ip + 2) new_registers program output
    | 2 ->
        let new_registers = (CharMap.add 'B' (combo operand mod 8)) registers in
        run (ip + 2) new_registers program output
    | 3 ->
        let a = CharMap.find 'A' registers in
        let new_ip = if a == 0 then ip + 2 else operand in
        run new_ip registers program output
    | 4 ->
        let new_registers =
          let b = CharMap.find 'B' registers in
          let c = CharMap.find 'C' registers in
          (CharMap.add 'B' (b lxor c)) registers
        in
        run (ip + 2) new_registers program output
    | 5 -> run (ip + 2) registers program ((combo operand mod 8) :: output)
    | 6 ->
        let new_registers =
          let a = CharMap.find 'A' registers in
          (CharMap.add 'B' (a / Base.Int.pow 2 (combo operand))) registers
        in
        run (ip + 2) new_registers program output
    | 7 ->
        let new_registers =
          let a = CharMap.find 'A' registers in
          (CharMap.add 'C' (a / Base.Int.pow 2 (combo operand))) registers
        in
        run (ip + 2) new_registers program output
    | _ -> failwith "Invalid opcode"

let solve_part_one input =
  let registers, program = parse input in
  run 0 registers program [] |> List.rev |> List.map string_of_int
  |> Core.String.concat ~sep:","

let solve_part_two _ = 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %s\nSolution 2: %d\n" solution1 solution2
