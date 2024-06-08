(** Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

    # insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)

let rec insert_at item index list =
  match list with
  | [] -> if index >= 0 then [ item ] else []
  | hd :: tl ->
    if index = 0
    then item :: insert_at item (index - 1) (hd :: tl)
    else hd :: insert_at item (index - 1) tl
;;

let%expect_test "Test insert_at 1" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a alfa b c d) |}]
;;

let%expect_test "Test insert_at 10" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = insert_at "alfa" 10 [ "a"; "b"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a b c d alfa) |}]
;;
