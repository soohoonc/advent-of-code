(* @see: https://adventofcode.com/2024/day/18 *)

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let parse input =
  input |> Core.String.split_lines
  |> List.map (fun s ->
         Core.String.split_on_chars ~on:[ ',' ] s |> List.map int_of_string
         |> function
         | f :: s :: _ -> (f, s)
         | _ -> failwith "invalid input")

let inbound (x, y) h w = x >= 0 && y >= 0 && x <= h && y <= w

let solve_part_one input =
  let input = parse input in
  let h = 70 and w = 70 and first = 1024 in
  let corrupted = ref PairMap.empty in
  let rec loop = function
    | n when n > first -> ()
    | n ->
        (corrupted := PairMap.(add (List.nth input n) true !corrupted));
        loop (n + 1)
  in
  loop 0;
  let dist = ref PairMap.empty in
  let rec dijkstras curr d =
    let cd = PairMap.(find_opt curr !dist) |> Option.value ~default:max_int in
    if inbound curr h w && (not PairMap.(mem curr !corrupted)) && cd > d then (
      (dist := PairMap.(add curr d !dist));
      dijkstras (fst curr + 1, snd curr) (d + 1);
      dijkstras (fst curr - 1, snd curr) (d + 1);
      dijkstras (fst curr, snd curr - 1) (d + 1);
      dijkstras (fst curr, snd curr + 1) (d + 1))
  in
  dijkstras (0, 0) 0;
  PairMap.(find_opt (h, w) !dist) |> Option.value ~default:max_int

let solve_part_two input =
  let input = parse input in
  let n = List.length input and h = 70 and w = 70 in
  let rec search i j =
    if i = j then List.nth input i
    else
      let m = (i + j) / 2 in
      let corrupted = ref PairMap.empty in
      let rec loop = function
        | n when n > m -> ()
        | n ->
            (corrupted := PairMap.(add (List.nth input n) true !corrupted));
            loop (n + 1)
      in
      loop 0;
      let dist = ref PairMap.empty in
      let rec dijkstras curr d =
        let cd =
          PairMap.(find_opt curr !dist) |> Option.value ~default:max_int
        in
        if inbound curr h w && (not PairMap.(mem curr !corrupted)) && cd > d
        then (
          (dist := PairMap.(add curr d !dist));
          dijkstras (fst curr + 1, snd curr) (d + 1);
          dijkstras (fst curr - 1, snd curr) (d + 1);
          dijkstras (fst curr, snd curr - 1) (d + 1);
          dijkstras (fst curr, snd curr + 1) (d + 1))
      in
      dijkstras (0, 0) 0;
      let d =
        PairMap.(find_opt (h, w) !dist) |> Option.value ~default:max_int
      in
      if d = max_int then search i m else search (m + 1) j
  in
  search 0 (n - 1)

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d,%d \n" solution1
    (fst solution2) (snd solution2)
