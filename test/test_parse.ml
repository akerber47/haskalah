open OUnit
;;

open Batteries
;;

(* Make sure the parser doesn't gag on the given file *)
let lex_and_parse f =
  let src = Util.file_to_string f in
  ignore(Parse.parse
    (Lex.unlayout src (Lex.postlex src (Lex.prelex src))))
;;

(* Make sure all our little test files parse without errors. *)
let test_parse_files _ = begin
  lex_and_parse "test/files/simple/hello.hs";
  lex_and_parse "test/files/simple/fib.hs";
  lex_and_parse "test/files/simple/fac.hs";
  lex_and_parse "test/files/euler/1.hs";
  lex_and_parse "test/files/euler/2.hs";
  lex_and_parse "test/files/euler/3.hs";
  lex_and_parse "test/files/euler/4.hs";
  lex_and_parse "test/files/euler/5.hs";
  lex_and_parse "test/files/euler/6.hs";
  lex_and_parse "test/files/euler/7.hs";
  lex_and_parse "test/files/euler/9.hs";
  lex_and_parse "test/files/euler/10.hs";
  lex_and_parse "test/files/euler/12.hs";
  lex_and_parse "test/files/euler/13.hs"
end
;;

(* Name the test cases and group them together *)
let suite =
  "parse test suite">:::
     ["parse_files">:: test_parse_files]
;;

let run_all () =
  run_test_tt_main suite
;;
