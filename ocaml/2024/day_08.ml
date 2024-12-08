(* @see: https://adventofcode.com/2024/day/8 *)

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module CharMap = Map.Make (Char)

let parse input =
  let lines =
    input
    |> Core.String.split_on_chars ~on:[ '\n'; '\r' ]
    |> List.filter (fun s -> String.trim s <> "")
  in
  let m = List.length lines in
  let n = if m > 0 then String.length (List.hd lines) else 0 in
  let coords, antenna =
    lines
    |> List.mapi (fun i line ->
           String.to_seq line
           |> Seq.mapi (fun j c ->
                  match c with '.' -> None | c -> Some (i, j, c))
           |> Seq.filter_map (fun x -> x)
           |> List.of_seq)
    |> List.concat
    |> List.fold_left
         (fun (coords, map) (x, y, c) ->
           let existing = CharMap.find_opt c map |> Option.value ~default:[] in
           (coords @ [ (x, y, c) ], CharMap.add c ((x, y) :: existing) map))
         ([], CharMap.empty)
  in
  (m, n, coords, antenna)

let solve_part_one input =
  let m, n, coords, antennas = parse input in
  let inbounds (x, y) = x >= 0 && y >= 0 && x < m && y < n in
  let rec antenna_loop acc seen = function
    | [] -> acc
    | antenna :: antennas_rest ->
        let x, y, antenna_type = antenna in
        let same_antennas =
          CharMap.(find_opt antenna_type antennas) |> Option.value ~default:[]
        in
        let get_antinode a1 a2 =
          let x1, y1 = a1 and x2, y2 = a2 in
          ((2 * x1) - x2, (2 * y1) - y2)
        in
        let rec get_antinodes acc seen = function
          | [] -> (acc, seen)
          | h :: t ->
              let antinode = get_antinode (x, y) h in
              let in_bounds = inbounds antinode in
              let already_seen =
                PairMap.(find_opt antinode seen) |> Option.value ~default:false
              in
              if in_bounds && (not (h = antinode)) && not already_seen then
                get_antinodes (acc + 1) PairMap.(add antinode true seen) t
              else get_antinodes acc seen t
        in
        let antinodes, new_seen = get_antinodes 0 seen same_antennas in
        antenna_loop (acc + antinodes) new_seen antennas_rest
  in
  antenna_loop 0 PairMap.empty coords

let solve_part_two input =
  let m, n, coords, antennas = parse input in
  let inbounds (x, y) = x >= 0 && y >= 0 && x < m && y < n in
  let rec antenna_loop acc seen = function
    | [] -> acc
    | antenna :: antennas_rest ->
        let x, y, antenna_type = antenna in
        let same_antennas =
          CharMap.(find_opt antenna_type antennas) |> Option.value ~default:[]
        in
        let get_antinode a1 a2 seen =
          let x1, y1 = a1 and x2, y2 = a2 in
          let rec loop i acc seen =
            let node = (x1 + (i * (x1 - x2)), y1 + (i * (y1 - y2))) in
            if (not (node = a1 && not (i = 0))) && inbounds node then
              let already_seen =
                PairMap.(find_opt node seen) |> Option.value ~default:false
              in
              loop (i + 1)
                (acc + if not already_seen then 1 else 0)
                PairMap.(add node true seen)
            else (acc, seen)
          in
          loop 0 0 seen
        in

        let rec get_antinodes acc seen = function
          | [] -> (acc, seen)
          | h :: t ->
              let antinodes, new_seen = get_antinode (x, y) h seen in
              get_antinodes (acc + antinodes) new_seen t
        in
        let antinodes, new_seen = get_antinodes 0 seen same_antennas in
        antenna_loop (acc + antinodes) new_seen antennas_rest
  in
  antenna_loop 0 PairMap.empty coords

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
