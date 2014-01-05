open Batteries

(* Using pre-existing parser generators is LAME, so this is a hand-rolled one.
 * What could possibly go wrong? *)

(* Types / functions a parser generator needs to know in order to build a
 * parser for a given grammar. Input type for Make *)
module type Parse_gen_able = sig
  type tm (* Terminals in grammar *)
  val tm_compare : tm -> tm -> int
  type ntm (* Nonterminals in grammar *)
  val ntm_compare : ntm -> ntm -> int
  type lx (* Lexeme input for generated parser *)
  val lx_to_tm : lx -> tm (* Extract terminal symbol info from lexeme *)
  val eof : tm (* End of file terminal *)
  type ast (* AST output for generated parser *)
end;;

(* Output type for Make. A fully typed-out parser generator. *)
module type P = sig

(* Types copied over from input module *)
type term
type nonterm
type lexeme
type ast

type symbol =
  | Terminal of term
  | Nonterminal of nonterm

type production = {
  lhs : nonterm;
  rhs : symbol list;
  (* Semantic action to be applied to the list of return values of the
   * semantic actions of the terms of rhs. *)
  semantic_action : (ast list -> ast);
}

type grammar = {
  goal : nonterm;
  productions : production Array.t;
  (* Semantic action to be applied when matching any terminal symbol. Taken as
   * input the matched lexeme (to extract its contents, line/col #s, etc. *)
  terminal_action : (lexeme -> ast);
}

module Term_set : Set.S with type elt = term

(* Represents a FIRST set. FIRST(string of grammar symbols) = the set of all
 * first terminals in terminal-strings that match the given symbol-string.
 * i.e., look at all possible grammar expansions of the given symbol-string.
 * The first elements of those expansions make up the FIRST set.  If the
 * symbol-string can expand to an empty terminal-string, we say that the FIRST
 * set also includes epsilon, in addition to ordinary terminals. *)
type first_set = {
  terms : Term_set.t; (* "Actual" (non-epsilon) terminals in the set. *)
  has_epsilon : bool; (* Whether or not the set includes epsilon *)
}

(* Items in the canonical LR(1) construction. An item represents a "possible
 * production match" for our parser as it reads in tokens (terminal symbols).
 * Sets of these (ie "all possible matches at this point") will be used to
 * construct states for our parser.
 * prod = production we are matching against (the rhs of)
 * dot = how far in the production we've matched so far (i.e. when we read
 *       another token, what's the next thing in rhs it must match)
 * lookahead = next thing we're matching against, after the rhs. Without this
 *             we'd be doing LR(0). *)
type item = {
  prod : int; (* Index into given array of productions *)
  dot : int; (* from 0 up to length of rhs of production *)
  lookahead : term;
}

module Item_set : Set.S with type elt = item

(* Represents the "canonical collection" (CC) of sets of items for our CFG.
 * These item sets will be the states of our parsing pushdown automaton. Each
 * item set stores items that match all the possible inputs the parser
 * is ready to receive at a given time. We also keep track of the transitions
 * between these item sets that take place when the parser processes an input
 * (ie a terminal in the grammar). *)
type cc = {
  itemsets : (int, Item_set.t) Map.t; (* Store each itemset with an index... *)
  gotos : (int * term, int) Map.t; (* so we can look up gotos by index *)
  num_itemsets : int;
}

(* States and tables of the resulting pushdown automaton. State numbers will
 * be the indices of the corresponding itemsets in the CC. *)
type state = int

type action =
  | Shift of state
  (* Store index of production (in grammar list) we use to reduce. *)
  | Reduce of int
  | Accept

(* Stores the actions for the pushdown automaton to take upon receiving a
 * terminal in a state - either "shift" (push+transition to a new state) or
 * "reduce" - (pop a bunch of old states, then apply a reduction).
 * Note that these actions also must be carried out on the actual asts when
 * parsing - i.e., shift pushes a new ast built from the most recent terminal,
 * and reduce combines the asts corresponding to the popped states. *)
type action_table = (state * term, action) Map.t
(* Stores the next state for the pushdown automaton to push+transition to after
 * performing a reduce action. We can't store these in the action table because
 * they depend on the state on the stack *underneath* all the popped states -
 * ie the things the parser will be ready to match *after* it's "completed a
 * submatch" corresponding to the reduced state. *)
type goto_table = (state * nonterm, state) Map.t

(* Compute the FIRST sets for nonterminals in a grammar. *)
val first_sets : grammar -> (nonterm, first_set) Map.t

(* Given the computed FIRST sets, find FIRST for an arbitrary input string
 * (list of symbols). *)
val first_set_string : (nonterm, first_set) Map.t ->
  symbol list -> first_set

(* Build the LR(1) canonical collection for a grammar. *)
val build_cc : grammar -> cc

(* Use the canonical collection to build the transition tables for the pushdown
 * automaton. The resulting automaton always has state 0 as the start state. *)
val build_tables : grammar -> cc -> action_table * goto_table

(* Simulate the resulting pushdown automaton. *)
val simulate : grammar -> action_table -> goto_table ->
    lexeme Queue.t -> ast

(* Overall function: basically applies the more detailed functions below in the
 * appropriate sequence to build the canonical collection and transition
 * tables, and simulate the resulting automaton. *)
val generate : grammar -> lexeme Queue.t -> ast

end;;

module Make : functor (Pga : Parse_gen_able) -> P
  with type term = Pga.tm
   and type nonterm = Pga.ntm
   and type lexeme = Pga.lx
   and type ast = Pga.ast
;;
