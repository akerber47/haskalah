open Batteries
;;
open Types
;;

(* Using pre-existing parser generators is LAME, so this is a hand-rolled one.
 * What could possibly go wrong? *)

(* The parser generator is divided into 2 components. The first takes in a
 * grammar and outputs transition table code needed to parse that grammar. The
 * second compiles that transition table code into a pushdown automaton
 * simulator needed to parse that grammar. We implement each of these as
 * functors (to make it easier to write test code with different terminal /
 * nonterminal types in our grammar), the first as Make_gen and
 * the second as Make_sim. *)

(* Types / functions a parser generator needs to know in order to build parsing
 * tables for a given grammar. Input type for Make_gen *)
module type Parse_gen_able = sig
  type tm (* Terminals in grammar *)
  type ntm (* Nonterminals in grammar *)
  val eof : tm (* End of file terminal *)
  (* Functions for printing debug output *)
  val tm_print : 'a BatIO.output -> tm -> unit
  val ntm_print : 'a BatIO.output -> ntm -> unit
end;;

(* Output type for Make_gen. Fully typed-out parser generator types needed for
 * table creation. *)
module type Pg = sig

(* Types copied over from input module *)
type term
type nonterm

type symbol =
  | T of term
  | NT of nonterm

type production = {
  lhs : nonterm;
  rhs : symbol list;
}

type grammar = {
  goal : nonterm;
  productions : production Array.t;
}

(* Given a grammar, computes the states and transition tables of the pushdown
 * automaton that can simulate the grammar. Write these (in ocaml source form)
 * into the given file as 2 functions,
 * computed_do_action and computed_do_goto *)
val output_tables : grammar -> string -> unit

end;;

module Make_gen : functor (Pga : Parse_gen_able) -> Pg
  with type term = Pga.tm
   and type nonterm = Pga.ntm
;;

(* Types / functions a parser generator needs to know in order to simulate the
 * operation of a parsing table for a given grammar (augmented with semantic
 * actions). *)
module type Parse_sim_able = sig
  type tm (* Terminals in grammar *)
  type ntm (* Nonterminals in grammar *)
  val eof : tm (* End of file terminal *)
  type lx (* Lexeme input for generated parser *)
  val lx_to_tm : lx -> tm (* Extract terminal symbol info from lexeme *)
  type ast (* AST output for generated parser *)
  (* Functions for printing debug output *)
  val lx_print : 'a BatIO.output -> lx -> unit
  val ast_print : 'a BatIO.output -> ast -> unit
end;;


(* Output type for Make_sim. Fully typed-out parser simulator once given
 * appropriate transition tables. *)
module type Ps = sig

(* Types copied over from input module *)
type term
type nonterm
type lexeme
type ast

type symbol =
  | T of term
  | NT of nonterm

type aug_production = {
  lhs : nonterm;
  rhs : symbol list;
  (* Semantic action to be applied to the list of return values of the
   * semantic actions of the terms of rhs. *)
  semantic_action : (ast list -> ast);
}

type aug_grammar = {
  goal : nonterm;
  productions : aug_production Array.t;
  (* Semantic action to be applied when matching any terminal symbol. Taken as
   * input the matched lexeme (to extract its contents, line/col #s, etc. *)
  terminal_action : (lexeme -> ast);
}

(* Given an augmented grammar and functions implementing the action and goto
 * tables of the pushdown automaton, simulates it to parse the given queue of
 * lexemes, building an AST. The input functions are presumably obtained by
 * running Pg.output_tables, though they could always be written by hand. *)
val simulate : aug_grammar -> (state * term -> action) ->
  (state * nonterm -> state) -> (lexeme -> ast) -> lexeme Queue.t -> ast

end;;

module Make_sim : functor (Psa : Parse_sim_able) -> Ps
  with type term = Psa.tm
   and type nonterm = Psa.ntm
   and type lexeme = Psa.lx
   and type ast = Psa.ast
;;
