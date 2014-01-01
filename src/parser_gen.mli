open Batteries

(* Using pre-existing parser generators is LAME, so this is a hand-rolled one.
 * What could possibly go wrong? *)

type symbol =
  | Terminal Lex.token
  | Nonterminal Parse.nonterm

type production = {
  lhs : Parse.nonterm;
  rhs : symbol list;
  (* Semantic action to be applied to the list of return values of the
   * semantic actions of the terms of rhs. *)
  action : (Parse.ast list -> Parse.ast);
}

type grammar = {
  productions : production list;
  (* Semantic action to be applied when matching any terminal symbol. Taken as
   * input the matched lexeme (to extract its contents, line/col #s, etc. *)
  terminal_action : (Lex.lexeme -> Parse.ast);
}

val generate : grammar -> Lex.lexeme Queue.t -> Parse.ast
