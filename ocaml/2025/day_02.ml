(* @see: https://adventofcode.com/2025/day/2 *)

let parse input =
  input |> Core.String.strip |> Core.String.split ~on:','
  |> List.map (fun id ->
         match Core.String.split ~on:'-' id with
         | [ a; b ] -> (int_of_string a, int_of_string b)
         | _ -> failwith "Invalid ID format")

let num_digits n = int_of_float (log10 (float_of_int n)) + 1

let mask_of_length ?(base = 10) length =
  int_of_float (float_of_int base ** float_of_int length)

let solve_part_one input =
  List.fold_left
    (fun sum (ida, idb) ->
      let valid ?(round = "down") value =
        let length = num_digits value in
        let half = length / 2 in
        let mask = mask_of_length half in
        if length mod 2 <> 0 && round = "down" then mask - 1
        else if length mod 2 <> 0 && round = "up" then mask
        else
          let fst_half = value / mask and snd_half = value mod mask in
          if fst_half > snd_half && round = "down" then fst_half - 1
          else if fst_half < snd_half && round = "up" then fst_half + 1
          else fst_half
      in
      let first = valid ~round:"up" ida and last = valid idb in
      let rec add acc = function
        | curr when curr > last -> acc
        | curr ->
            let length = num_digits curr in
            let mask = mask_of_length length in
            add (acc + (curr * mask) + curr) (curr + 1)
      in
      sum + add 0 first)
    0 (parse input)

module IntMap = Map.Make (Int)

let solve_part_two input =
  let build_repeated_value length sublength value =
    let rec loop acc = function
      | i when i = length / sublength -> acc
      | i -> loop ((mask_of_length sublength * acc) + value) (i + 1)
    in
    loop value 1
  in
  List.fold_left
    (fun sum (ida, idb) ->
      let length_a = num_digits ida and length_b = num_digits idb in
      let rec length_loop acc = function
        | i when i > length_b -> acc
        | i ->
            let found = ref IntMap.empty in
            let rec sublength_loop acc' = function
              | j when j > i / 2 -> acc'
              | j ->
                  if i mod j <> 0 then sublength_loop acc' (j + 1)
                  else
                    let start = mask_of_length (j - 1)
                    and stop = mask_of_length j - 1 in
                    let rec values_loop acc'' = function
                      | k when k > stop -> acc''
                      | k ->
                          let value = build_repeated_value i j k in
                          let seen =
                            match IntMap.find_opt value !found with
                            | None -> false
                            | Some x -> x
                          in
                          if value > idb then acc''
                          else if value < ida || seen then
                            values_loop acc'' (k + 1)
                          else (
                            found := IntMap.add value true !found;
                            values_loop (acc'' + value) (k + 1))
                    in
                    sublength_loop (values_loop acc' start) (j + 1)
            in
            length_loop (acc + sublength_loop 0 1) (i + 1)
      in
      sum + length_loop 0 length_a)
    0 (parse input)

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
