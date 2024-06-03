(** Rotate a list N places to the left.

    # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
    - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)

let rotate l n =
  let p_1, p_2 = Prob_17.split l n in
  p_2 @ p_1
;;

let%expect_test "Test rotate 3" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (d e f g h a b c) |}]
;;
