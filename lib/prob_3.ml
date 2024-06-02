(** Find the N'th element of a list.
    # List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
    - : string = "c"
      # List.nth ["a"] 2;;
      Exception: Failure "nth". *)

exception Nth of string

let rec nth_inner list target_index cur_index =
  if cur_index >= List.length list
  then raise (Nth "nth")
  else if target_index = cur_index
  then List.hd list
  else nth_inner (List.tl list) target_index (cur_index + 1)
;;

let nth list index = nth_inner list index 0
