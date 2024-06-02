(** Find the number of elements of a list.

    OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)

let rec length_tail l acc =
  match l with
  | [] -> acc
  | _ :: tl -> length_tail tl (acc + 1)
;;

let length l = length_tail l 0

let%expect_test "Test full int list" =
  Printf.printf "%d" (length [ 1; 2; 3; 4 ]);
  [%expect {| 4 |}]
;;

let%expect_test "Test full char list" =
  Printf.printf "%d" (length [ 'a'; 'b' ]);
  [%expect {| 2 |}]
;;

let%expect_test "Test empty list" =
  Printf.printf "%d" (length []);
  [%expect {| 0 |}]
;;
