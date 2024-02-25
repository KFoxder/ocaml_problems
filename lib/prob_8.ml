(*
   Eliminate consecutive duplicates of list elements.

   # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let rec remove_dups_inner list seen acc =
  match list with
  | [] -> acc
  | hd :: tail ->
    if List.mem hd seen
    then remove_dups_inner tail seen acc
    else remove_dups_inner tail [ hd ] (hd :: acc)
;;

let compress l =
  let rev_list = remove_dups_inner l [] [] in
  List.rev rev_list
;;
