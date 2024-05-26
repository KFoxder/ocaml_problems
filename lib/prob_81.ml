(*
   081
   Write a function paths g a b that returns all acyclic path p from node a to node b ≠ a in the graph g. 
   The function should return the list of all paths via backtracking.
   # let example_graph =
    {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;

    val example_graph : char graph_term =
    {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]}

    # paths example_graph 'f' 'b';;
    - : char list list = [['f'; 'c'; 'b']; ['f'; 'b']]
*)

type g =
  { nodes : char list
  ; edges : (char * char) list
  }

module Chars = struct
  type t = char

  let compare x y = Stdlib.compare x y
end

module CharSet = Set.Make (Chars)

let rec paths_inner
  (graph : g)
  (cur_node : char)
  (end_node : char)
  (seen : CharSet.t)
  (cur_path : char list)
  =
  if cur_node = end_node (* Check if found node *)
  then [ List.append cur_path [ cur_node ] ]
  else if CharSet.mem cur_node seen (* Check if we have already seen this node *)
  then []
  else (
    (* Find all edges that start with this cur_node *)
    let results = ref [] in
    List.iter
      (fun edge ->
        let edge_in, edge_out = edge in
        if edge_in = cur_node
        then (
          let seen = CharSet.add cur_node seen in
          let cur_path = List.append cur_path [ cur_node ] in
          let res = paths_inner graph edge_out end_node seen cur_path in
          if not (List.is_empty res) then results := List.append res !results else ())
        else ())
      graph.edges;
    !results)
;;

let paths (graph : g) (start_node : char) (end_node : char) =
  if List.mem start_node graph.nodes && List.mem end_node graph.nodes
  then (
    (* Only find path if both char exist in graph *)
    let seen = CharSet.empty in
    paths_inner graph start_node end_node seen [])
  else []
;;
