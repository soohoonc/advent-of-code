(* @see: https://adventofcode.com/2024/day/15 *)

let parse input =
  match Str.split (Str.regexp "\n\n") input with
  | [ map_str; actions_str ] ->
      let map =
        Core.String.split_on_chars ~on:[ '\n' ] map_str
        |> List.filter (fun s -> String.empty <> s)
      in
      let actions =
        actions_str |> String.trim |> String.to_seq |> List.of_seq
        |> List.filter (fun s -> ' ' <> s && s <> '\n')
      in
      (map, actions)
  | _ -> failwith "Invalid input format"

let find c map m n =
  let rec loop x y =
    if x >= m then failwith "Robot not found!"
    else if y >= n then loop (x + 1) 0
    else
      let c' = Core.String.get (List.nth map x) y in
      if c' = c then (x, y) else loop x (y + 1)
  in
  loop 0 0

type direction = { dx : int; dy : int }

let direction_map = function
  | '>' -> { dx = 0; dy = 1 }
  | '<' -> { dx = 0; dy = -1 }
  | 'v' -> { dx = 1; dy = 0 }
  | '^' -> { dx = -1; dy = 0 }
  | _ -> failwith "Invalid direction"

let swap map coord1 coord2 =
  let x1, y1 = coord1 and x2, y2 = coord2 in
  let c1 = Core.String.get (List.nth map x1) y1 in
  let c2 = Core.String.get (List.nth map x2) y2 in
  List.mapi
    (fun i row ->
      if i = x1 && i = x2 then
        String.mapi
          (fun j c -> if j = y1 then c2 else if j = y2 then c1 else c)
          row
      else if i = x1 then String.mapi (fun j c -> if j = y1 then c2 else c) row
      else if i = x2 then String.mapi (fun j c -> if j = y2 then c1 else c) row
      else row)
    map

let solve_part_one input =
  let map, action = parse input in
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let apply_action map x y action =
    let dir = direction_map action in
    let dx = dir.dx and dy = dir.dy in
    let next_tile = Core.String.get (List.nth map (x + dx)) (y + dy) in
    let next_free map start_x start_y dx dy =
      let rec scan x y =
        match Core.String.get (List.nth map x) y with
        | '#' -> None
        | '.' -> Some (x, y)
        | 'O' -> scan (x + dx) (y + dy)
        | l ->
            failwith
              (Printf.sprintf
                 "Unrecognized Character %c: Finding Next Free Slot" l)
      in
      scan start_x start_y
    in
    match next_tile with
    | '.' -> (swap map (x, y) (x + dx, y + dy), x + dx, y + dy)
    | '#' -> (map, x, y)
    | 'O' -> (
        match next_free map (x + dx) (y + dy) dx dy with
        | Some (x', y') ->
            let map' = swap map (x + dx, y + dy) (x', y') in
            let map'' = swap map' (x, y) (x + dx, y + dy) in
            (map'', x + dx, y + dy)
        | None -> (map, x, y))
    | l ->
        failwith (Printf.sprintf "Unrecognized Character %c: Applying Action" l)
  in
  let sum_coords map =
    let rec loop x y acc =
      if x >= m then acc
      else if y >= n then loop (x + 1) 0 acc
      else
        let n =
          if Core.String.get (List.nth map x) y = 'O' then (x * 100) + y else 0
        in
        loop x (y + 1) (acc + n)
    in
    loop 0 0 0
  in
  let start_x, start_y = find '@' map m n in
  let rec loop map x y = function
    | [] -> (map, x, y)
    | h :: t ->
        let map', x', y' = apply_action map x y h in
        loop map' x' y' t
  in
  let final_map, _, _ = loop map start_x start_y action in
  List.iter (fun line -> Printf.printf "%s\n" line) final_map;
  sum_coords final_map

let mutate_map map =
  List.map
    (fun row ->
      String.to_seq row
      |> Seq.map (fun c ->
             if c = '.' then ".."
             else if c = '#' then "##"
             else if c = 'O' then "[]"
             else "@.")
      |> List.of_seq |> String.concat "")
    map

let solve_part_two input =
  let map, action = parse input in
  let map = mutate_map map in
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let apply_action map x y action =
    let dir = direction_map action in
    let dx = dir.dx and dy = dir.dy in
    let next_tile = Core.String.get (List.nth map (x + dx)) (y + dy) in
    let moveable map start_x start_y dx dy =
      let rec scan x y =
        match Core.String.get (List.nth map x) y with
        | '#' -> None
        | '.' -> Some (x, y)
        | '[' | ']' ->
            scan (x + dx) (y + dy) (* TODO calculate whole size of cluster *)
        | l ->
            failwith
              (Printf.sprintf
                 "Unrecognized Character %c: Finding Next Free Slot" l)
      in
      scan start_x start_y
    in
    (* TODO move the entire cluster s*)
    let shift map start_x start_y dx dy = () in
    match next_tile with
    | '.' -> (swap map (x, y) (x + dx, y + dy), x + dx, y + dy)
    | '#' -> (map, x, y)
    | '[' | ']' -> (
        match moveable map (x + dx) (y + dy) dx dy with
        | Some _ ->
            let map' = shift map (x + dx) (y + dy) dx dy in
            (swap map' (x, y) (x + dx, y + dy), x + dx, y + dy)
        | None -> (map, x, y))
    | l ->
        failwith (Printf.sprintf "Unrecognized Character %c: Applying Action" l)
  in
  let sum_coords map =
    let rec loop x y acc =
      if x >= m then acc
      else if y >= n then loop (x + 1) 0 acc
      else
        let n =
          if Core.String.get (List.nth map x) y = '[' then (x * 100) + y else 0
        in
        (* if Core.String.get (List.nth map x) y = 'O' then
           Printf.printf "O at (%d,%d) - score: %d\n" x y ((x * 100) + y); *)
        loop x (y + 1) (acc + n)
    in
    loop 0 0 0
  in
  let start_x, start_y = find '@' map m n in
  let rec loop map x y = function
    | [] -> (map, x, y)
    | h :: t ->
        let map', x', y' = apply_action map x y h in
        loop map' x' y' t
  in
  let final_map, _, _ = loop map start_x start_y action in
  List.iter (fun line -> Printf.printf "%s\n" line) final_map;
  sum_coords final_map

let solve input =
  (* let solve _ = *)
  (* let input =
       "########\n\
        #..O.O.#\n\
        ##@.O..#\n\
        #...O..#\n\
        #.#.O..#\n\
        #...O..#\n\
        #......#\n\
        ########\n\n\
        <^^>>>vv<v>>v<<"
     in *)
  (* let input =
       "##########\n\
        #..O..O.O#\n\
        #......O.#\n\
        #.OO..O.O#\n\
        #..O@..O.#\n\
        #O#..O...#\n\
        #O..O..O.#\n\
        #.OO.O.OO#\n\
        #....O...#\n\
        ##########\n\n\
        <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
        vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
        ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
        <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
        ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
        ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
        >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
        <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
        ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
        v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
     in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in

  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
