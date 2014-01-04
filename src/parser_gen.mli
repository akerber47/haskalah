open Batteries

(* Using pre-existing parser generators is LAME, so this is a hand-rolled one.
 * What could possibly go wrong? *)

type symbol =
  | Terminal of Parse.term
  | Nonterminal of Parse.nonterm

type production = {
  lhs : Parse.nonterm;
  rhs : symbol list;
  (* Semantic action to be applied to the list of return values of the
   * semantic actions of the terms of rhs. *)
  semantic_action : (Parse.ast list -> Parse.ast);
}

type grammar = {
  goal : Parse.nonterm;
  productions : production Array.t;
  (* Semantic action to be applied when matching any terminal symbol. Taken as
   * input the matched lexeme (to extract its contents, line/col #s, etc. *)
  terminal_action : (Parse.term -> Parse.ast);
}

(* Types used to construct FIRST sets (and I suppose also FOLLOW sets, except
 * FOLLOW sets aren't used in LR) *)
module TermSet : Set.S with type elt = Parse.term

type first_set = {
  terms : TermSet.t; (* "Actual" (non-epsilon) terminals in the set. *)
  has_epsilon : bool; (* Whether or not the set includes epsilon *)
}

(* Items in the LR(1) construction *)
type item = {
  prod : int; (* Index into given array of productions *)
  dot : int; (* from 0 up to length of rhs of production *)
  lookahead : Parse.term;
}

module ItemSet : Set.S with type elt = item

type cc = {
  itemsets : (int, ItemSet.t) Map.t; (* Store each itemset with an index... *)
  gotos : (int * Parse.term, int) Map.t; (* so we can look up gotos by index *)
}

(* States and tables of the resulting pushdown automaton. State numbers will
 * be the indices of the corresponding itemsets. *)
type state = int

(* Note that we also need to store the corresponding semantic actions on the
 * output tree together with each reduce action, so we know what to do with the
 * "actual stuff" (not just parse terminal / nonterminal types) on the stack *)
type action =
  | Shift of state
  | Reduce of state * (Parse.ast list -> Parse.ast)

type action_table = (state * Parse.term, action) Map.t
type goto_table = (state * Parse.nonterm, state) Map.t

(* Compute the FIRST sets for nonterminals in a grammar. *)
val first_sets : grammar -> (Parse.nonterm, first_set) Map.t

(* Given the computed FIRST sets, find FIRST for an arbitrary input string
 * (list of symbols). *)
val first_set_string : (Parse.nonterm, first_set) Map.t ->
  symbol list -> first_set

(* Build the LR(1) canonical collection for a grammar. *)
val build_cc : grammar -> cc

(* Use the canonical collection to build the transition tables for the pushdown
 * automaton. The resulting automaton always has state 0 as the start state. *)
val build_tables : grammar -> cc -> action_table * goto_table

(* Simulate the resulting pushdown automaton. *)
val simulate : grammar -> action_table -> goto_table ->
    Parse.term Queue.t -> Parse.ast

(* Overall function: basically applies the more detailed functions below in the
 * appropriate sequence to build the canonical collection and transition
 * tables, and simulate the resulting automaton. *)
val generate : grammar -> Parse.term Queue.t -> Parse.ast
