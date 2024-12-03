(* @see: https://adventofcode.com/2024/day/3 *)

let solve_part_one input =
  let chars = Core.String.to_list input in

  let rec read_number chars acc =
    match chars with
    | c :: rest when c >= '0' && c <= '9' ->
        read_number rest (acc ^ String.make 1 c)
    | rest -> (int_of_string acc, rest)
  in

  let rec parse_token acc = function
    | [] -> acc
    | 'm' :: 'u' :: 'l' :: '(' :: rest -> (
        let num1, rest1 = read_number rest "" in
        match rest1 with
        | ',' :: rest2 -> (
            let num2, rest3 = read_number rest2 "" in
            match rest3 with
            | ')' :: rest4 -> parse_token ((num1 * num2) + acc) rest4
            | _ -> parse_token acc rest3)
        | _ -> parse_token acc rest1)
    | _ :: rest -> parse_token acc rest
  in
  parse_token 0 chars

let solve_part_two input =
  let chars = Core.String.to_list input in
  let rec read_number chars acc =
    match chars with
    | c :: rest when c >= '0' && c <= '9' ->
        read_number rest (acc ^ String.make 1 c)
    | rest -> (int_of_string acc, rest)
  in

  let rec parse_token enabled acc = function
    | [] -> acc
    | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: rest ->
        parse_token false acc rest
    | 'd' :: 'o' :: '(' :: ')' :: rest -> parse_token true acc rest
    | 'm' :: 'u' :: 'l' :: '(' :: rest -> (
        let num1, rest1 = read_number rest "" in
        match rest1 with
        | ',' :: rest2 -> (
            let num2, rest3 = read_number rest2 "" in
            match rest3 with
            | ')' :: rest4 ->
                parse_token enabled
                  ((num1 * num2 * if enabled then 1 else 0) + acc)
                  rest4
            | _ -> parse_token enabled acc rest3)
        | _ -> parse_token enabled acc rest1)
    | _ :: rest -> parse_token enabled acc rest
  in
  parse_token true 0 chars

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
