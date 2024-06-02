(** Find out whether a list is a palindrome.

    Hint: A palindrome is its own reverse.

    # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
    - : bool = true
      # not (is_palindrome ["a"; "b"]);;
    - : bool = true *)

let rec is_pal_inner l r =
  match l, r with
  | [], [] | [], _ | _, [] -> true
  | h1 :: tl1, h2 :: tl2 -> if h1 <> h2 then false else is_pal_inner tl1 tl2
;;

let is_palindrom l = is_pal_inner l (List.rev l)

let%expect_test "Test is palindrom" =
  let result = is_palindrom [ "x"; "a"; "x" ] in
  Printf.printf "%b" result;
  [%expect {| true |}]
;;

let%expect_test "Test is not palindrom" =
  let result = is_palindrom [ "e"; "x"; "a"; "x" ] in
  Printf.printf "%b" result;
  [%expect {| false |}]
;;
