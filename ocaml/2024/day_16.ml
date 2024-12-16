(* @see: https://adventofcode.com/2024/day/16 *)

let parse input =
  input |> Core.String.split_lines |> List.filter (fun s -> String.empty <> s)

let find map char =
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let rec loop x y =
    if x >= m then
      failwith (Printf.sprintf "Character %c could not be found" char)
    else if y >= n then loop (x + 1) 0
    else
      let curr = Core.String.get (List.nth map x) y in
      if curr = char then (x, y) else loop x (y + 1)
  in
  loop 0 0

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let inbounds map x y =
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  x >= 0 && y >= 0 && x < m && y < n

let solve_part_one input =
  let map = parse input in
  let s = find map 'S' and e = find map 'E' in
  let dp = ref PairMap.empty in
  let rec find x y dx dy score =
    let rotate dir = function
      | 0, 1 -> if dir = 1 then (1, 0) else (-1, 0)
      | 0, -1 -> if dir = 1 then (-1, 0) else (1, 0)
      | 1, 0 -> if dir = 1 then (0, -1) else (0, 1)
      | -1, 0 -> if dir = 1 then (0, 1) else (0, -1)
      | _ -> failwith "Invalid diretion"
    in
    if (not (inbounds map x y)) || Core.String.get (List.nth map x) y = '#' then
      ()
    else
      let curr_score =
        PairMap.(find_opt (x, y) !dp) |> Option.value ~default:max_int
      in
      if curr_score <= score then ()
      else (
        (dp := PairMap.(add (x, y) (min score curr_score) !dp));
        let dx1, dy1 = rotate (-1) (dx, dy) and dx2, dy2 = rotate 1 (dx, dy) in
        find (x + dx) (y + dy) dx dy (score + 1);
        find (x + dx1) (y + dy1) dx1 dy1 (score + 1001);
        find (x + dx2) (y + dy2) dx2 dy2 (score + 1001))
  in

  find (fst s) (snd s) 0 1 0;
  PairMap.(find_opt e !dp) |> Option.value ~default:max_int

let solve_part_two input =
  let map = parse input in
  let s = find map 'S' and e = find map 'E' in
  let dp = ref PairMap.empty in
  let rec find x y dx dy score =
    let rotate dir = function
      | 0, 1 -> if dir = 1 then (1, 0) else (-1, 0)
      | 0, -1 -> if dir = 1 then (-1, 0) else (1, 0)
      | 1, 0 -> if dir = 1 then (0, -1) else (0, 1)
      | -1, 0 -> if dir = 1 then (0, 1) else (0, -1)
      | _ -> failwith "Invalid diretion"
    in
    if (not (inbounds map x y)) || Core.String.get (List.nth map x) y = '#' then
      ()
    else
      let curr_score =
        PairMap.(find_opt (x, y) !dp) |> Option.value ~default:max_int
      in
      if curr_score <= score then ()
      else (
        (dp := PairMap.(add (x, y) (min score curr_score) !dp));
        let dx1, dy1 = rotate (-1) (dx, dy) and dx2, dy2 = rotate 1 (dx, dy) in
        find (x + dx) (y + dy) dx dy (score + 1);
        find (x + dx1) (y + dy1) dx1 dy1 (score + 1001);
        find (x + dx2) (y + dy2) dx2 dy2 (score + 1001))
  in
  (* This is a bad solution, it depends which diretion you start you search in *)
  let rec backtrack curr e prev seen =
    match curr with
    | curr when curr = e -> seen
    | _ -> (
        let score =
          PairMap.(find_opt curr !dp) |> Option.value ~default:max_int
        in
        let neighbors =
          [
            (fst curr - 1, snd curr);
            (fst curr + 1, snd curr);
            (fst curr, snd curr + 1);
            (fst curr, snd curr - 1);
          ]
        in
        let valid_next =
          List.filter
            (fun pos ->
              let next_score =
                PairMap.(find_opt pos !dp) |> Option.value ~default:max_int
              in
              let is_valid =
                (not (PairMap.mem pos seen))
                && (next_score = score - 1
                   || next_score = score - 1001
                   || next_score = prev - 2)
              in
              is_valid)
            neighbors
        in

        let seen' = PairMap.add curr 1 seen in
        match valid_next with
        | [] -> seen'
        | _ ->
            List.fold_left
              (fun acc pos -> backtrack pos e score acc)
              seen' valid_next)
  in
  find (fst s) (snd s) 0 1 0;
  let seen =
    backtrack e s
      (PairMap.(find_opt e !dp) |> Option.value ~default:max_int)
      PairMap.empty
  in
  PairMap.cardinal seen + 1

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
