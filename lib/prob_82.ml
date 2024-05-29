(**
   Write a functions cycle g a that returns a closed path (cycle) p starting at a given node a in the graph g. The predicate should return the list of all cycles via backtracking.

# let example_graph =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;
val example_graph : char graph_term =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]}
# cycles example_graph 'f';;
- : char list list =
[['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f'];
   ['f'; 'b'; 'f']; ['f'; 'k'; 'f']]
*)

module Chars = struct
  type t = char

  let compare x y = Stdlib.compare x y
end

module CharSet = Set.Make (Chars)

type 'a graph_term =
  { nodes : 'a list
  ; edges : ('a * 'a) list
  }

let get_neighbors graph node =
  List.filter_map
    (fun (a, b) -> if a = node then Some b else if b = node then Some a else None)
    graph.edges
;;

let rec cycles_inner graph target_node cur_node acc seen =
  if target_node = cur_node && not (List.is_empty acc)
  then [ [ cur_node ] @ acc ]
  else if CharSet.mem cur_node seen
  then []
  else (
    let seen = CharSet.add cur_node seen in
    let neighbors = get_neighbors graph cur_node in
    let results = ref [] in
    List.iter
      (fun neighbor ->
        let result = cycles_inner graph target_node neighbor ([ cur_node ] @ acc) seen in
        if List.is_empty result then () else results := result @ !results)
      neighbors;
    !results)
;;

let cycles graph node = cycles_inner graph node node [] CharSet.empty
