(** Replicate the elements of a list a given number of times.

    # replicate ["a"; "b"; "c"] 3;;
    - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)

let rec expand item num =
  match num with
  | 0 -> []
  | _ -> item :: expand item (num - 1)
;;

let rec duplicate_inner l n acc =
  match l with
  | [] -> acc
  | hd :: tl -> duplicate_inner tl n (acc @ expand hd n)
;;

let replicate l n = duplicate_inner l n []

let%expect_test "Test list" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = replicate [ "a"; "b"; "c" ] 3 in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (a a a b b b c c c) |}]
;;
