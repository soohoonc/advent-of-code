(* @see: https://adventofcode.com/2024/day/18 *)

module CharMap = Map.Make (Char)

module TrieNode = struct
  type trie_node = { children : trie_node CharMap.t; mutable is_leaf : bool }

  let empty () = { children = CharMap.empty; is_leaf = false }
end

module Trie = struct
  type trie = TrieNode.trie_node

  let empty () = TrieNode.empty ()

  let add trie s =
    let rec insert node i =
      if i = String.length s then (
        node.TrieNode.is_leaf <- true;
        node)
      else
        let c = s.[i] in
        let child =
          match CharMap.(find_opt c node.TrieNode.children) with
          | Some child -> child
          | None -> TrieNode.empty ()
        in
        let child = insert child (i + 1) in
        { node with children = CharMap.add c child node.TrieNode.children }
    in
    insert trie 0

  let contains ?(prefix = false) trie s =
    let rec search node i =
      if i = String.length s then prefix || node.TrieNode.is_leaf
      else
        match CharMap.(find_opt s.[i] node.TrieNode.children) with
        | Some child -> search child (i + 1)
        | None -> false
    in
    search trie 0
end

let parse input =
  input |> Core.String.split_lines |> List.filter (fun s -> String.empty <> s)
  |> function
  | patterns :: t ->
      let trie = Trie.empty () in
      let trie =
        Core.String.split_on_chars ~on:[ ','; ' ' ] patterns
        |> List.filter (fun s -> String.empty <> s)
        |> List.fold_left (fun acc p -> Trie.add acc p) trie
      in
      (trie, t)
  | _ -> failwith "invalid input"

let solve_part_one input =
  let patterns, towels = parse input in
  let rec valid towel i j =
    let n = String.length towel in
    if i >= n then true
    else if j >= n then false
    else
      let l =
        if Trie.contains patterns (String.sub towel i (j - i + 1)) then
          (* Printf.printf "valid pattern %s\n" (String.sub towel i (j - i + 1)); *)
          valid towel (j + 1) (j + 1)
        else false
      in
      let r =
        if Trie.contains patterns (String.sub towel i (j - i + 1)) ~prefix:true
        then valid towel i (j + 1)
        else false
      in
      l || r
  in
  List.fold_left
    (fun acc towel -> acc + if valid towel 0 0 then 1 else 0)
    0 towels

let solve_part_two _input = 0

let solve input =
  (* let solve _ =
     let input =
       "r, wr, b, g, bwu, rb, gb, br\n\n\
        brwrr\n\
        bggr\n\
        gbbr\n\
        rrbgbr\n\
        ubwu\n\
        bwurrg\n\
        brgr\n\
        bbrgwb"
     in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d \n" solution1 solution2
