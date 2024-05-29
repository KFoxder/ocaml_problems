(** Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

    Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

    # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : string rle list =
      [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let get_item cur_item cur_count =
  match cur_count with
  | 1 -> One cur_item
  | x -> Many (x, cur_item)
;;

let rec encode_inner strings cur_item cur_count =
  match strings with
  | [] -> [ get_item cur_item cur_count ]
  | hd :: tl ->
    if hd = cur_item
    then encode_inner tl hd (cur_count + 1)
    else get_item cur_item cur_count :: encode_inner tl hd 1
;;

let encode strings =
  if List.is_empty strings then [] else encode_inner (List.tl strings) (List.hd strings) 1
;;
