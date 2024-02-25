(*
   Flatten a nested list structure.

   type 'a node =
   | One of 'a
   | Many of 'a node list
   # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten list acc =
  match list with
  | [] -> acc
  | hd :: tail ->
    let acc_ =
      match hd with
      | One a -> List.append acc [ a ]
      | Many list_ -> flatten list_ acc
    in
    flatten tail acc_
;;
