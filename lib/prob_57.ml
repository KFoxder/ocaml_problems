(*
   Construct a binary search tree from a list of integer numbers.

   # construct [3; 2; 5; 7; 1];;
   - : int binary_tree =
     Node (3, Node (2, Node (1, Empty, Empty), Empty),
     Node (5, Empty, Node (7, Empty, Empty)))
*)

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t

let rec insert node item =
  match node with
  | Empty -> Empty
  | Node (value, l, r) ->
    if item <= value
    then (
      match l with
      | Empty -> Node (value, Node (item, Empty, Empty), r)
      | _ -> Node (value, insert l item, r))
    else (
      match r with
      | Empty -> Node (value, l, Node (item, Empty, Empty))
      | _ -> Node (value, l, insert r item))
;;

let construct l =
  if List.length l < 1
  then Empty
  else (
    let head = ref (Node (List.hd l, Empty, Empty)) in
    let rest = List.tl l in
    List.iter (fun item -> head := insert !head item) rest;
    !head)
;;
