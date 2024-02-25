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

let () =
  run_test_tt_main suite_1;
  run_test_tt_main suite_7;
  run_test_tt_main suite_8
;;
