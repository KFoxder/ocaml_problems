(** # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : (int * string) list =
      [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

let rec encode_inner strings cur_item cur_count =
  match strings with
  | [] -> [ cur_count, cur_item ]
  | head :: rest ->
    if head = cur_item
    then encode_inner rest cur_item (cur_count + 1)
    else (cur_count, cur_item) :: encode_inner rest head 1
;;

let encode (strings : string list) : (int * string) list =
  if List.is_empty strings then [] else encode_inner (List.tl strings) (List.hd strings) 1
;;
