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
  action : (Parse.ast list -> Parse.ast);
}

type grammar = {
  goal : Parse.nonterm;
  productions : production Array.t;
  (* Semantic action to be applied when matching any terminal symbol. Taken as
   * input the matched lexeme (to extract its contents, line/col #s, etc. *)
  terminal_action : (Parse.term -> Parse.ast);
}

(* Overall function: basically applies the more detailed functions below in the
 * appropriate sequence to build the canonical collection and transition
 * tables, and simulate the resulting automaton. *)
val generate : grammar -> Parse.term Queue.t -> Parse.ast

(* Items in the LR(1) construction *)
type item = {
  prod : int;
  dot : int;
  lookahead : Parse.term;
}

module ItemSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = item
  end
)

module ItemCollection = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = ItemSet.t * int
  end
)

(* States and tables of the resulting pushdown automaton. *)
type state = int

type action =
  | Shift of state
  | Reduce of state
type action_table = (state * Parse.term, action) Map.t
type goto_table = (state * Parse.nonterm, state) Map.t

(* Build the LR(1) canonical collection for a grammar. *)
val build_cc : grammar -> ItemCollection.t

(* Use the canonical collection to build the transition tables for the pushdown
 * automaton. The resulting automaton always has state 0 as the start state. *)
val build_tables : grammar -> ItemCollection.t -> action_table * goto_table

(* Simulate the resulting pushdown automaton. TODO this isn't quite right - how
 * do we apply the resulting semantic actions? *)
val simulate : action_table -> goto_table -> Parse.term Queue.t -> Parse.ast
