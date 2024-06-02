(** Reverse a list.

    OCaml standard library has List.rev but we ask that you reimplement it.

    # rev ["a"; "b"; "c"];;
    - : string list = ["c"; "b"; "a"] *)

let rec rev_inner (l : 'a list) (acc : 'a list) =
  match l with
  | [] -> acc
  | hd :: tl -> rev_inner tl (List.append [ hd ] acc)
;;

let rev l = rev_inner l []

let%expect_test "Test int list" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = rev [ 1; 2; 3; 4 ] in
  let result_string = [%sexp_of: int list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (4 3 2 1) |}]
;;
