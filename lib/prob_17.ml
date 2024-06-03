(** Split a list into two parts; the length of the first part is given.

    If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

    # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
    - : string list * string list =
      (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
      # split ["a"; "b"; "c"; "d"] 5;;
    - : string list * string list = (["a"; "b"; "c"; "d"], []) *)

let split list num =
  let first_part = ref [] in
  let second_part = ref [] in
  let counter = ref num in
  List.iter
    (fun item ->
      if !counter > 0
      then (
        first_part := !first_part @ [ item ];
        counter := !counter - 1)
      else second_part := !second_part @ [ item ])
    list;
  !first_part, !second_part
;;

let%expect_test "Test split 3" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3 in
  let result_string = [%sexp_of: string list * string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| ((a b c)(d e f g h i j)) |}]
;;

let%expect_test "Test split 5" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = split [ "a"; "b"; "c"; "d" ] 5 in
  let result_string = [%sexp_of: string list * string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| ((a b c d)()) |}]
;;
