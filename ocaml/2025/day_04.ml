(* @see: https://adventofcode.com/2025/day/4 *)

let parse input = input |> Core.String.split_lines

let directions =
  [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) ]

let count_adj ~map ~bounds:(m, n) ~pos:(x, y) =
  List.fold_left
    (fun acc direction ->
      let x' = x + fst direction and y' = y + snd direction in
      if x' < 0 || y' < 0 || x' >= m || y' >= n then acc
      else acc + Bool.to_int (String.get (List.nth map x') y' = '@'))
    0 directions

let solve_part_one input =
  let input = parse input in
  let m = List.length input and n = String.length (List.nth input 0) in
  let rec row_loop acc = function
    | i when i = m -> acc
    | i ->
        let rec col_loop acc' = function
          | j when j = n -> acc'
          | j when String.get (List.nth input i) j <> '@' ->
              col_loop acc' (j + 1)
          | j ->
              let count = count_adj ~map:input ~bounds:(m, n) ~pos:(i, j) in
              col_loop (acc' + Bool.to_int (count < 4)) (j + 1)
        in
        row_loop (acc + col_loop 0 0) (i + 1)
  in
  row_loop 0 0

let update_list list x y new_char =
  List.mapi
    (fun i item ->
      if i = x then String.mapi (fun j c -> if j = y then new_char else c) item
      else item)
    list

let solve_part_two input =
  let input = ref (parse input) in
  let changes = ref true in
  let m = List.length !input and n = String.length (List.nth !input 0) in
  let rec iteration acc =
    match !changes with
    | false -> acc
    | true ->
        changes := false;
        let rec row_loop acc' = function
          | i when i = m -> acc'
          | i ->
              let rec col_loop acc'' = function
                | j when j = n -> acc''
                | j when String.get (List.nth !input i) j <> '@' ->
                    col_loop acc'' (j + 1)
                | j ->
                    let count =
                      count_adj ~map:!input ~bounds:(m, n) ~pos:(i, j)
                    in
                    if count < 4 then (
                      input := update_list !input i j '.';
                      changes := true)
                    else ();
                    col_loop (acc'' + Bool.to_int (count < 4)) (j + 1)
              in
              row_loop (acc' + col_loop 0 0) (i + 1)
        in
        iteration (acc + row_loop 0 0)
  in
  iteration 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
