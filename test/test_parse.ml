open OUnit2
;;

open Batteries
;;

(* Make sure the parser doesn't gag on the given file *)
let lex_and_parse f =
  let src = Util.file_to_string f in
  ignore(Util.dbg "%a" print_guess (Parse_check.postparse_check (Parse.parse
    (Lex.unlayout src (Lex.postlex src (Lex.prelex src))))))
;;

let make_test_list name lst =
  name >:::
  List.map
    (fun (f,arg) ->
      let test_title =
        Printf.sprintf "%s" arg
      in
        test_title >::
        (fun _ -> f arg))
    lst
;;

(* Make sure all our little test files parse without errors. *)
let parse_file_tests = make_test_list "lex_and_parse_files" [
  (lex_and_parse, "test/files/simple/hello.hs");
  (lex_and_parse, "test/files/simple/fib.hs");
  (lex_and_parse, "test/files/simple/fac.hs");
  (lex_and_parse, "test/files/euler/1.hs");
  (lex_and_parse, "test/files/euler/2.hs");
  (lex_and_parse, "test/files/euler/3.hs");
  (lex_and_parse, "test/files/euler/4.hs");
  (lex_and_parse, "test/files/euler/5.hs");
  (lex_and_parse, "test/files/euler/6.hs");
  (lex_and_parse, "test/files/euler/7.hs");
  (lex_and_parse, "test/files/euler/9.hs");
  (lex_and_parse, "test/files/euler/10.hs");
  (lex_and_parse, "test/files/euler/12.hs");
  (lex_and_parse, "test/files/euler/13.hs");
]
;;


(* Name the test cases and group them together *)
let suite =
  "parse test suite">:::
     [parse_file_tests]
;;

let run_all () =
  run_test_tt_main suite
;;
