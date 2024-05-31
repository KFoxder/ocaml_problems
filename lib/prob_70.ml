(** Multiway Trees *)

(**   -a
      /|\
      f c b
      |  / \
      g d   e

      T ('a', [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
      afg^^c^bd^e^^^ *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree_inner (tree : 'a mult_tree) (depth : int) : string =
  match tree with
  | T (c, t) ->
    let node_string = Char.escaped c in
    let children_to_string = List.map (fun i -> string_of_tree_inner i (depth + 1)) t in
    node_string ^ List.fold_left ( ^ ) "" children_to_string ^ "^"
;;

let string_of_tree (tree : 'a mult_tree) : string =
  (* Take in a type ('a mult_tree) and then turn it into a string *)
  string_of_tree_inner tree 0
;;

let rec tree_of_string_inner (s : string) (index : int) (stack : 'a mult_tree Stack.t) =
  let c = String.get s index in
  if c = '^'
  then (
    let child = Stack.pop stack in
    (* If we have gotten to the point of an empty stack then
       we know we are done parsing and return the parent element *)
    if Stack.is_empty stack
    then child
    else (
      (* If we encounter a '^' then we pop the parent off the stack
         and add our child to its children list and continue parsing *)
      let parent = Stack.pop stack in
      match parent with
      | T (char, children) ->
        let new_children = children @ [ child ] in
        let new_parent = T (char, new_children) in
        Stack.push new_parent stack;
        tree_of_string_inner s (index + 1) stack))
  else (
    (* If we encounter a char then just add it our stack and continue
       to parse *)
    let new_element = T (c, []) in
    Stack.push new_element stack;
    tree_of_string_inner s (index + 1) stack)
;;

let tree_of_string (s : string) : 'a mult_tree =
  let stack = Stack.create () in
  tree_of_string_inner s 0 stack
;;
