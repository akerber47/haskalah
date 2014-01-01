open OUnit
;;

open Batteries
;;

(* Check that lexing an input string gives the correct tokens, at least. *)
let assert_eqtoks toklist src =
  assert_equal ~cmp:(List.eq (=))
               ~printer:dump
               toklist
               (List.map (fun plx -> plx.Lex.pretoken)
                         (List.of_enum (Queue.enum (Lex.prelex src))))
;;

let test1 _ = assert_eqtoks [] "";;

let test2 _ = assert_eqtoks [Lex.PreQVarId ; Lex.PreSpecial] "x(";;

(* Name the test cases and group them together *)
let suite =
  "suite">:::
     ["test1">:: test1;
       "test2">:: test2]
;;

let run_all () =
  run_test_tt_main suite
;;
