open OUnit2

let list_printer list =
  let s = ref "" in
  List.iter (fun item -> s := !s ^ "|" ^ item) list;
  !s
;;

let nested_list_printer_string l_of_l =
  let s = ref "" in
  List.iter (fun nested_item -> List.iter (fun item -> s := !s ^ item) nested_item) l_of_l;
  !s
;;

let nested_list_printer_char l_of_l =
  let s = ref "" in
  List.iter
    (fun nested_item ->
      List.iter (fun item -> s := !s ^ Char.escaped item) nested_item;
      s := !s ^ "|")
    l_of_l;
  !s
;;

let suite_1 =
  "Tests Problem 1"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_1 in
          let l = last [ "a"; "b"; "c"; "d" ] in
          match l with
          | None -> assert_failure "Should not be None"
          | Some s -> assert_equal "d" s)
       ; ("test2"
          >:: fun _ ->
          let open Ocaml_problems.Prob_1 in
          let l = last [] in
          match l with
          | None -> ()
          | Some _ -> assert_failure "Should not have value")
       ]
;;

let suite_7 =
  "Tests Problem 7"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ] in
          let flatten_list = flatten list [] in
          let expected_list = [ "a"; "b"; "c"; "d"; "e" ] in
          assert_equal expected_list flatten_list)
       ; ("test2"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [ One "a" ] in
          let flatten_list = flatten list [] in
          let expected_list = [ "a" ] in
          assert_equal expected_list flatten_list)
       ; ("test3"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [] in
          let flatten_list = flatten list [] in
          let expected_list = [] in
          assert_equal expected_list flatten_list)
       ; ("test4"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [ Many [ One "c"; One "d" ] ] in
          let flatten_list = flatten list [] in
          let expected_list = [ "c"; "d" ] in
          assert_equal expected_list flatten_list)
       ]
;;

let suite_8 =
  "Tests Problem 8"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_8 in
          let list =
            [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
          in
          let compressed_list = compress list in
          let expected_list = [ "a"; "b"; "c"; "a"; "d"; "e" ] in
          assert_equal expected_list compressed_list)
       ]
;;

let suite_9 =
  "Tests Problem 9"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_9 in
          let list =
            [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
          in
          let packed_list = pack list in
          let expected_list =
            [ [ "a"; "a"; "a"; "a" ]
            ; [ "b" ]
            ; [ "c"; "c" ]
            ; [ "a"; "a" ]
            ; [ "d"; "d" ]
            ; [ "e"; "e"; "e"; "e" ]
            ]
          in
          assert_equal ~printer:nested_list_printer_string expected_list packed_list)
       ]
;;

let suite_49 =
  "Tests Problem 49"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_49 in
          let gray_list = gray 1 in
          let expected_list = [ "0"; "1" ] in
          assert_equal ~printer:list_printer expected_list gray_list)
       ; ("test2"
          >:: fun _ ->
          let open Ocaml_problems.Prob_49 in
          let gray_list = gray 2 in
          let expected_list = [ "00"; "01"; "11"; "10" ] in
          assert_equal ~printer:list_printer expected_list gray_list)
       ; ("test3"
          >:: fun _ ->
          let open Ocaml_problems.Prob_49 in
          let gray_list = gray 3 in
          let expected_list =
            [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]
          in
          assert_equal ~printer:list_printer expected_list gray_list)
       ]
;;

let suite_57 =
  "Tests Problem 57"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_57 in
          let list = [ 3; 2; 5; 7; 1 ] in
          let tree = construct list in
          let expected_tree =
            Node
              ( 3
              , Node (2, Node (1, Empty, Empty), Empty)
              , Node (5, Empty, Node (7, Empty, Empty)) )
          in
          assert_equal expected_tree tree)
       ]
;;

let suite_67 =
  "Tests Problem 67"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_67 in
          let tree =
            Node
              ( 'a'
              , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
              , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) )
          in
          let tree_string = string_of_tree tree in
          let expected_string = "a(b(d,e),c(,f(g,)))" in
          assert_equal expected_string tree_string)
       ]
;;

let suite_81 =
  "Tests Problem 81"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_81 in
          let example_graph =
            { nodes = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k' ]
            ; edges = [ 'h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b' ]
            }
          in
          let paths = paths example_graph 'f' 'b' in
          let expected_paths = [ [ 'f'; 'c'; 'b' ]; [ 'f'; 'b' ] ] in
          assert_equal ~printer:nested_list_printer_char expected_paths paths)
       ; ("test2"
          >:: fun _ ->
          let open Ocaml_problems.Prob_81 in
          let example_graph =
            { nodes = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k' ]
            ; edges = [ 'h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b' ]
            }
          in
          let paths = paths example_graph 'z' 'b' in
          let expected_paths = [] in
          assert_equal ~printer:nested_list_printer_char expected_paths paths)
       ; ("test3"
          >:: fun _ ->
          let open Ocaml_problems.Prob_81 in
          let example_graph =
            { nodes = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k' ]
            ; edges = [ 'h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b' ]
            }
          in
          let paths = paths example_graph 'b' 'z' in
          let expected_paths = [] in
          assert_equal ~printer:nested_list_printer_char expected_paths paths)
       ]
;;

let () =
  run_test_tt_main suite_1;
  run_test_tt_main suite_7;
  run_test_tt_main suite_8;
  run_test_tt_main suite_9;
  run_test_tt_main suite_49;
  run_test_tt_main suite_57;
  run_test_tt_main suite_67;
  run_test_tt_main suite_81
;;
