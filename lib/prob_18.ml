(** Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

    # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
    - : string list = ["c"; "d"; "e"; "f"; "g"] *)

let rec slice_inner list start stop cur acc =
  match list with
  | [] -> acc
  | hd :: tl ->
    if cur >= start && cur <= stop
    then slice_inner tl start stop (cur + 1) (hd :: acc)
    else slice_inner tl start stop (cur + 1) acc
;;

let slice l start stop = List.rev (slice_inner l start stop 0 [])

let%expect_test "Test slice" =
  let open Sexplib in
  let open Sexplib.Std in
  let result = slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6 in
  let result_string = [%sexp_of: string list] result |> Sexp.to_string in
  print_endline result_string;
  [%expect {| (c d e f g) |}]
;;
