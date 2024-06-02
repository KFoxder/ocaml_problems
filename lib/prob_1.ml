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

let%expect_test "Test 1: Valid element" =
  let result = Option.get (last [ 1; 2; 3; 4 ]) in
  Printf.printf "%d" result;
  [%expect {| 4 |}]
;;

let%expect_test "Test 2: Invalid" =
  let result = last [] in
  if Option.is_none result then print_endline "None" else print_endline "Is not None";
  [%expect {| None |}]
;;
