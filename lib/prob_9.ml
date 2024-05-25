(*
   009

   # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
   - : string list list =
     [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

let rec pack_inner list last_chars acc =
  match list with
  | [] -> last_chars :: acc
  | head :: tail ->
    (match last_chars with
     | [] -> pack_inner tail [ head ] acc
     | last_char :: _ ->
       if last_char = head
       then pack_inner tail (head :: last_chars) acc
       else pack_inner tail [ head ] (last_chars :: acc))
;;

let pack list =
  let result = pack_inner list [] [] in
  List.rev result
;;
