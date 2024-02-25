(*
   Somebody represents binary trees as strings of the following type (see example): "a(b(d,e),c(,f(g,)))".

   Write an OCaml function string_of_tree which generates this string representation, if the tree is given as usual (as Empty or Node(x,l,r) term). Then write a function tree_of_string which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single function tree_string which can be used in both directions.
*)

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t

let rec string_of_tree root =
  match root with
  | Empty -> ""
  | Node (a, l, r) ->
    (match l, r with
     | Empty, Empty -> Char.escaped a
     | _, _ -> Char.escaped a ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")")
;;
