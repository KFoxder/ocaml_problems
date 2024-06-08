(** If first argument is greater than second, produce a list in decreasing order.

    # range 4 9;;
    - : int list = [4; 5; 6; 7; 8; 9] *)

let rec range a b =
  if a = b then [ a ] else if a > b then b :: range a (b + 1) else a :: range (a + 1) b
;;

let%expect_test "Test range 4 9" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = range 4 9 in
  let result_string = [%sexp_of: int list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a alfa b c d) |}]
;;

let%expect_test "Test range 9 4 " =
  let open Sexplib in
  let open Sexplib.Std in
  let result = range 9 4 in
  let result_string = [%sexp_of: int list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a alfa b c d) |}]
;;

let%expect_test "Test range 4 4" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = range 4 4 in
  let result_string = [%sexp_of: int list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a alfa b c d) |}]
;;
