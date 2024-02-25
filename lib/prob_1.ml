(*
   001_tail

   Write a function `last : 'a list -> 'a option` that returns the last element of a list

   ```ocaml
   # last ["a" ; "b" ; "c" ; "d"];;
   - : string option = Some "d"
     # last [];;
   - : 'a option = None
     ```
*)

let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;
