(** Duplicate the elements of a list.

    # duplicate ["a"; "b"; "c"; "c"; "d"];;
    - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

let rec duplicate_inner l acc =
  match l with
  | [] -> acc
  | hd :: tl -> duplicate_inner tl (acc @ [ hd; hd ])
;;

let duplicate l = duplicate_inner l []

let%expect_test "Test list" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = duplicate [ "a"; "b"; "c"; "c"; "d" ] in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a a b b c c c c d d) |}]
;;
