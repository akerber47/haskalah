open OUnit
;;

open Batteries
;;

open Sheep_grammar
;;

open Calc_grammar
;;

(* This is to check that our parser generator works correctly. To check this,
 * we have a right-recursive, very simple grammar Sheep (for "Sheep Noises"),
 * and a slightly less simple left-recursive grammar Calc (for arithmetic
 * expressions). Rather than building ASTs, which are messy to check for
 * equality, the semantic actions specified in the grammars are very simple.
 * The Sheep grammar counts up the number of Baas, and the Calc grammar
 * computes the result of the computation. *)
let check_sheep_ast src_toklist output_ast =
  assert_equal ~cmp:((=))
               ~printer:dump
               output_ast
               (sheep_parser (Queue.of_enum (List.enum src_toklist)))
;;

let check_calc_ast src_toklist output_ast =
  assert_equal ~cmp:((=))
               ~printer:dump
               output_ast
               (calc_parser (Queue.of_enum (List.enum src_toklist)))
;;

let test_sheep_basic _ = begin
  check_sheep_ast [Baa;Sheep_grammar.EOF] 1;
  check_sheep_ast [Baa;Baa;Baa;Sheep_grammar.EOF] 3;
  check_sheep_ast [Baa;Baa;Baa;Baa;Baa;Baa;Sheep_grammar.EOF] 6;
end
;;

let test_calc_basic _ = begin
  check_calc_ast [(Num,1);(Plus,-1);(Num,1);(Calc_grammar.EOF,-1)] 2;
  check_calc_ast [(Num,7);(Plus,-1);(Num,4);(Times,-1);
    (Num,4);(Calc_grammar.EOF,-1)] 23;
  check_calc_ast [(Num,1);(Times,-1);(Num,1);(Plus,-1);(Num,2);(Times,-1);
    (Num,2);(Times,-1);(Num,2);(Plus,-1);(Num,1);(Calc_grammar.EOF,-1)] 10;
end
;;

(* Name the test cases and group them together *)
let suite =
  "parse test suite">:::
    ["test_sheep_basic">:: test_sheep_basic;
     "test_calc_basic">:: test_calc_basic]
;;

let run_all () =
  run_test_tt_main suite
;;
