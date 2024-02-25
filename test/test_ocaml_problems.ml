open OUnit2

let suite_1 =
  "Tests Problem 1"
  >::: [ ("test1"
          >:: fun _ ->
          let open Ocaml_problems.Prob_1 in
          let l = last [ "a"; "b"; "c"; "d" ] in
          match l with
          | None -> assert_failure "Should not be None"
          | Some s -> assert_equal s "d")
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
          assert_equal flatten_list expected_list)
       ; ("test2"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [ One "a" ] in
          let flatten_list = flatten list [] in
          let expected_list = [ "a" ] in
          assert_equal flatten_list expected_list)
       ; ("test3"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [] in
          let flatten_list = flatten list [] in
          let expected_list = [] in
          assert_equal flatten_list expected_list)
       ; ("test4"
          >:: fun _ ->
          let open Ocaml_problems.Prob_7 in
          let list = [ Many [ One "c"; One "d" ] ] in
          let flatten_list = flatten list [] in
          let expected_list = [ "c"; "d" ] in
          assert_equal flatten_list expected_list)
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
          assert_equal compressed_list expected_list)
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
          assert_equal tree expected_tree)
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
          assert_equal tree_string expected_string)
       ]
;;

let () =
  run_test_tt_main suite_1;
  run_test_tt_main suite_7;
  run_test_tt_main suite_8;
  run_test_tt_main suite_57;
  run_test_tt_main suite_67
;;
