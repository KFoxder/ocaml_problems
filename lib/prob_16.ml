(** Drop every N'th element from a list.

    # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
    - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)

let rec drop_inner l num target =
  match l with
  | [] -> []
  | hd :: tl ->
    if num = 1 then drop_inner tl target target else hd :: drop_inner tl (num - 1) target
;;

let drop l num = drop_inner l num num

let%expect_test "Test drop 3" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3 in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a b d e g h j) |}]
;;

let%expect_test "Test drop 0" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 0 in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a b c d e f g h i j) |}]
;;
