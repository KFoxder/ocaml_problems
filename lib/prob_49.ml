(*
   049

   An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

   n = 1: C(1) = ['0', '1'].
   n = 2: C(2) = ['00', '01', '11', '10'].
   n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].
   Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code.

   # gray 1;;
   - : string list = ["0"; "1"]
     # gray 2;;
   - : string list = ["00"; "01"; "11"; "10"]
     # gray 3;;
   - : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
*)

let add_value_to_items list value = List.map (fun item -> value ^ item) list

let rec gray_inner num acc =
  (*
     1. Count down number
     2. Add 0 and 1 to each existing number in the list
  *)
  match num with
  | 0 -> acc
  | x ->
    let part_one = add_value_to_items acc "0" in
    let part_two = add_value_to_items acc "1" in
    let new_acc = List.append part_one (List.rev part_two) in
    gray_inner (x - 1) new_acc
;;

let gray num = if num < 0 then [] else gray_inner num [ "" ]
