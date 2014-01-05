open OUnit
;;

open Batteries
;;

open Sheep_grammar
;;

(* Check that gives the correct ast, at least. *)
let check_sheep_ast src_toklist output_ast =
  assert_equal ~cmp:((=))
               ~printer:dump
               output_ast
               (sheep_parser (Queue.of_enum (List.enum src_toklist)))
;;

let test_sheep_basic _ = begin
  check_sheep_ast [Baa] 1;
  check_sheep_ast [Baa;Baa;Baa] 3;
  check_sheep_ast [Baa;Baa;Baa;Baa;Baa;Baa] 6;
end
;;


(* Name the test cases and group them together *)
let suite =
  "parse test suite">:::
     ["test_sheep_basic">:: test_sheep_basic]
;;

let run_all () =
  run_test_tt_main suite
;;
