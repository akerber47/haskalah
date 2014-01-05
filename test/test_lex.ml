open OUnit
;;

open Batteries
;;

open Lex
;;

(******** prelex tests *********)

(* Check that prelex an input string gives the correct tokens, at least. *)
let check_pretoks src toklist =
  assert_equal ~cmp:(List.eq (=))
               ~printer:dump
               toklist
               (List.map (fun plx -> plx.pretoken)
                         (List.of_enum (Queue.enum (prelex src))))
;;

let test_prelex_basic _ = begin
  check_pretoks "" [];
  check_pretoks "x(" [PreQVarId ; PreSpecial];
  check_pretoks "foo BarBAZ! 1 2.3 '4' \"five\" {six} "
                [PreQVarId; PreQVarId; PreQVarSym; PreIntLit;
                PreFloatLit; PreCharLit; PreStringLit; PreSpecial;
                PreQVarId; PreSpecial];
end
;;

(******** postlex tests, also test prelex indirectly *********)
(* Check tokens and contents of the tokens. *)
let check_toks_cnts src toklist cntslist = begin
  assert_equal ~cmp:(List.eq (=))
               ~printer:dump
               toklist
               (List.map (fun lx -> lx.token)
                   (List.of_enum (Queue.enum (postlex src (prelex src)))));
  assert_equal ~cmp:(List.eq (=))
               ~printer:dump
               cntslist
               (List.map (fun lx -> lx.Lex.contents)
                   (List.of_enum (Queue.enum (postlex src (prelex src)))))
end

(* Make sure all token types end / transition correctly. *)
let test_lex_endtok _ = begin
  check_toks_cnts "abc123" [VarId] ["abc123"];
  check_toks_cnts "123abc" [IntLit; VarId] ["123"; "abc"];
  (* Qualified names *)
  check_toks_cnts "x.y'z" [VarId; VarSym; VarId] ["x"; "."; "y'z"];
  check_toks_cnts "X.y'z" [QVarId] ["X.y'z"];
  check_toks_cnts "X.Y'z" [QConId] ["X.Y'z"];
  check_toks_cnts "X.Y.z" [QConId; VarSym; VarId] ["X.Y"; "."; "z"];
  check_toks_cnts "X.Y.Z" [QConId; VarSym; ConId] ["X.Y"; "."; "Z"];
  check_toks_cnts "x.Y.Z" [VarId; VarSym; QConId] ["x"; "."; "Y.Z"];
  check_toks_cnts "M." [ConId; VarSym] ["M"; "."];
  check_toks_cnts "M.." [QVarSym] ["M.."];
  check_toks_cnts "M.1" [ConId; VarSym; IntLit] ["M"; "."; "1"];
  check_toks_cnts "M...." [QVarSym] ["M...."];
  check_toks_cnts "M.:.:" [QConSym] ["M.:.:"];
  (* Names and operators *)
  check_toks_cnts "x+1" [VarId; VarSym; IntLit] ["x"; "+"; "1"];
  check_toks_cnts "x+-1" [VarId; VarSym; IntLit] ["x"; "+-"; "1"];
  check_toks_cnts "x + -1" [VarId; VarSym; VarSym; IntLit]
      ["x"; "+"; "-"; "1"];
  (* Integer literals *)
  check_toks_cnts "0123" [IntLit] ["0123"];
  check_toks_cnts "0o123" [IntLit] ["0o123"];
  check_toks_cnts "0x123" [IntLit] ["0x123"];
  check_toks_cnts "0xdef" [IntLit] ["0xdef"];
  check_toks_cnts "0odef" [IntLit; VarId] ["0"; "odef"];
  check_toks_cnts "0o189def" [IntLit; IntLit; VarId] ["0o1"; "89"; "def"];
  check_toks_cnts "0xdefghi" [IntLit; VarId] ["0xdef"; "ghi"];
  check_toks_cnts "0xghi" [IntLit; VarId] ["0"; "xghi"];
  (* Floating point literals *)
  check_toks_cnts "1.2" [FloatLit] ["1.2"];
  check_toks_cnts "1..2" [IntLit; RDotDot; IntLit] ["1"; ".."; "2"];
  check_toks_cnts "1.2e" [FloatLit; VarId] ["1.2"; "e"];
  check_toks_cnts "1.2e+" [FloatLit; VarId; VarSym] ["1.2"; "e"; "+"];
  check_toks_cnts "1.2e3" [FloatLit] ["1.2e3"];
  check_toks_cnts "1.2e+03" [FloatLit] ["1.2e+03"];
  check_toks_cnts "1.2E-3" [FloatLit] ["1.2E-3"];
  check_toks_cnts "1.2e-3+4" [FloatLit; VarSym; IntLit] ["1.2e-3"; "+"; "4"];
  check_toks_cnts "1.2e0f0" [FloatLit; VarId] ["1.2e0"; "f0"];
  check_toks_cnts "0x1.2" [IntLit; VarSym; IntLit] ["0x1"; "."; "2"];
  check_toks_cnts "1.0x2" [FloatLit; VarId] ["1.0"; "x2"];
  check_toks_cnts "1.2e+0x2" [FloatLit; VarId] ["1.2e+0"; "x2"];
end
;;

(* Name the test cases and group them together *)
let suite =
  "lex test suite">:::
     ["prelex_basic">:: test_prelex_basic;
       "lex_endtok">:: test_lex_endtok]
;;

let run_all () =
  run_test_tt_main suite
;;
