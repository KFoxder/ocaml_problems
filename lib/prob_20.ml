(** Remove the K'th element from a list.

    The first element of the list is numbered 0, the second 1,...

    # remove_at 1 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "c"; "d"] *)

let rec remove_at index list =
  match list with
  | [] -> []
  | hd :: tl ->
    if index = 0 then remove_at (index - 1) tl else hd :: remove_at (index - 1) tl
;;

let%expect_test "Test remove index 1" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = remove_at 1 [ "a"; "b"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a c d) |}]
;;

let%expect_test "Test remove index 0" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = remove_at 0 [ "a"; "b"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (b c d) |}]
;;

let%expect_test "Test remove index too large" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = remove_at 5 [ "a"; "b"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a b c d) |}]
;;
