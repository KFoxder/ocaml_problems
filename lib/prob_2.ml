(** Find the last but one (last and penultimate) elements of a list.
    # last_two ["a"; "b"; "c"; "d"];;
    - : (string * string) option = Some ("c", "d")
      # last_two ["a"];;
    - : (string * string) option = None *)

let rec last_two l =
  match l with
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl
;;
