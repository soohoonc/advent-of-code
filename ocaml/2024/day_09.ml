(* @see: https://adventofcode.com/2024/day/9 *)

let parse input = String.trim input

let rec get_incremental_checksum id start_idx end_idx acc =
  match (start_idx, end_idx) with
  | s, e when s >= e -> acc
  | _ ->
      (* print_endline
         (Printf.sprintf "Adding to checksum id: %d, val: %d" start_idx
            (id * start_idx)); *)
      get_incremental_checksum id (start_idx + 1) end_idx
        (acc + (id * start_idx))

let solve_part_one input =
  let input = parse input in
  let length = Core.String.length input in
  let rec compute_checksum idx i j r_left f_left acc =
    (* print_endline
       (Printf.sprintf
          "compute_checksum idx: %d, i: %d, j: %d, r_left: %d, f_left: %d, acc: \
           %d"
          idx i j r_left f_left acc); *)
    if i >= j then
      acc
      +
      if i = j && r_left > 0 then
        get_incremental_checksum (i / 2) idx (idx + r_left) 0
      else 0
    else if f_left = 0 then
      let i = if i mod 2 = 0 then i else i + 1 in
      let id = i / 2 in
      let new_acc, new_idx =
        let files =
          int_of_string (Core.String.make 1 (Core.String.get input i))
        in
        let a = get_incremental_checksum id idx (idx + files) 0 in
        (acc + a, idx + files)
      in
      let free_space =
        int_of_string (Core.String.make 1 (Core.String.get input (i + 1)))
      in
      compute_checksum new_idx (i + 1) j r_left free_space new_acc
    else
      let id = j / 2 in
      let new_acc, new_idx, new_i, new_j, new_r_left, new_f_left =
        if f_left >= r_left then
          let next_r_left =
            int_of_string (Core.String.make 1 (Core.String.get input (j - 2)))
          in
          let a = get_incremental_checksum id idx (idx + r_left) 0 in
          (acc + a, idx + r_left, i, j - 2, next_r_left, f_left - r_left)
        else
          let a = get_incremental_checksum id idx (idx + f_left) 0 in
          (acc + a, idx + f_left, i + 1, j, r_left - f_left, 0)
      in
      compute_checksum new_idx new_i new_j new_r_left new_f_left new_acc
  in

  compute_checksum 0 0 (length - 1)
    (int_of_string (Core.String.make 1 (Core.String.get input (length - 1))))
    0 0

module IntMap = Map.Make (Int)

let solve_part_two _ = 0
(* let solve_part_two input =
   let input = parse input in
   let length = Core.String.length input in
   let rec compute_checksum idx i j seen acc =
     (* print_endline
        (Printf.sprintf
           "compute_checksum idx: %d, i: %d, j: %d, r_left: %d, f_left: %d, acc: \
            %d"
           idx i j r_left f_left acc); *)
     if
     else if f_left = 0 then
       let i = if i mod 2 = 0 then i else i + 1 in
       let id = i / 2 in
       let new_acc, new_idx =
         let files =
           int_of_string (Core.String.make 1 (Core.String.get input i))
         in
         let a = get_incremental_checksum id idx (idx + files) 0 in
         (acc + a, idx + files)
       in
       let free_space =
         int_of_string (Core.String.make 1 (Core.String.get input (i + 1)))
       in
       compute_checksum new_idx (i + 1) j r_left free_space new_acc
     else
       let id = j / 2 in
       let new_acc, new_idx, new_i, new_j, new_r_left, new_f_left =
         if f_left >= r_left then
           let next_r_left =
             int_of_string (Core.String.make 1 (Core.String.get input (j - 2)))
           in
           let a = get_incremental_checksum id idx (idx + r_left) 0 in
           (acc + a, idx + r_left, i, j - 2, next_r_left, f_left - r_left)
         else
           let a = get_incremental_checksum id idx (idx + f_left) 0 in
           (acc + a, idx + f_left, i + 1, j, r_left - f_left, 0)
       in
       compute_checksum new_idx new_i new_j new_r_left new_f_left new_acc
   in

   compute_checksum 0 0 (length - 1)
     (int_of_string (Core.String.make 1 (Core.String.get input (length - 1))))
     0 0 *)

let solve input =
  (* let solve _ = *)
  (* let input = "12345" in *)
  (* let input = "2333133121414131402" in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
