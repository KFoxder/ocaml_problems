(** 095

    On financial documents, like cheques, numbers must sometimes be written in full words. Example: 175 must be written as one-seven-five. Write a function full_words to print (non-negative) integer numbers in full words.

    # full_words 175;;
    - : string = "one-seven-five" *)

let num_to_word n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | _ -> ""
;;

let rec num_to_list num =
  if num < 10
  then [ num ]
  else (
    let digit = num mod 10 in
    let remainder = num / 10 in
    num_to_list remainder @ [ digit ])
;;

let full_words num =
  let res = ref "" in
  let nums = num_to_list num in
  List.iteri
    (fun index item ->
      if index = 0
      then res := !res ^ num_to_word item
      else res := !res ^ "-" ^ num_to_word item)
    nums;
  !res
;;
