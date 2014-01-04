open Batteries
;;

(* Unfortunately this Haskell grammar fails very, very badly at being LL(1)
 * (it has lots and lots of infix operators, can't tell tuples from parentheses
 * or lists from list comprehensions, etc.). It might be convertible to LL(1)
 * but that would take tons of factoring. So we'll implement bottom-up parsing.
 * LR(1). *)

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
module TermSet =  Set.Make (
  struct
    let compare = Pervasives.compare
    type t = Parse.term
  end
)

type first_set = {
  terms : TermSet.t; (* "Actual" terminals in the set. *)
  has_epsilon : bool; (* Whether or not the set includes epsilon *)
}

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

let first_sets cfg =
  let curmap = ref Map.empty
  and all_nonterms =
      List.unique (Array.to_list (Array.map (fun p -> p.lhs) cfg.productions))
  in let rec do_nonterm nextnt =
    if Map.mem nextnt !curmap then
      ()
    else begin
      (* Get all possible RHS productions *)
      let rhss =
          Array.map (fun p -> p.rhs)
                    (Array.filter (fun p -> p.lhs = nextnt) cfg.productions)
      (* Find the possible first set elems off a given string (symbol list), and
       * add them into the given first set. Return new first set. *)
      in let rec add_first_from_str fs symbls =
        match symbls with
        (* If we reach empty list, all nonterminals above us in the string had
         * an epsilon (empty) production. *)
        | [] -> { fs with has_epsilon = true }
        | (Terminal t)::_ -> { fs with terms = TermSet.add t fs.terms }
        | (Nonterminal nt)::ss -> begin
          (* XXX Grammar had better not be left-recursive (!) *)
          do_nonterm nt;
          let firstfs = Map.find nt !curmap in
          (* If the first symbol is nonterminal (potentially) reducing to
           * epsilon, we also need to check the 2nd symbol, and so on -
           * otherwise we can just look at the possible terminals. *)
          if firstfs.has_epsilon then
            add_first_from_str
                { fs with terms = TermSet.union firstfs.terms fs.terms }
                ss
          else
            { fs with terms = TermSet.union firstfs.terms fs.terms }
        end
      in
      (* Find all possible first set elems from productions of this nonterminal
       * in the grammar. *)
      let nextfs = Array.fold_left add_first_from_str
          { terms = TermSet.empty; has_epsilon = false; } rhss
      in
      (* And add the resulting first set to the map. ("memoize") *)
      curmap := Map.add nextnt nextfs !curmap
    end
  in begin
    List.iter do_nonterm all_nonterms;
    !curmap
  end
;;

(* Same as middle loop above, but without memoizing / building table. *)
let first_set_string fstable symbls =
  let rec first_set_string_acc syms fs =
    match syms with
    | [] -> { fs with has_epsilon = true }
    | (Terminal t)::_ -> { fs with terms = TermSet.add t fs.terms }
    | (Nonterminal nt)::ss ->
      let firstfs = Map.find nt fstable in
      (* If the first symbol is nonterminal (potentially) reducing to
       * epsilon, we also need to check the 2nd symbol, and so on -
       * otherwise we can just look at the possible terminals. *)
      if firstfs.has_epsilon then
        first_set_string_acc ss
            { fs with terms = TermSet.union firstfs.terms fs.terms }
      else
        { fs with terms = TermSet.union firstfs.terms fs.terms }
  in
  first_set_string_acc symbls
      { terms = TermSet.empty; has_epsilon = false; }
;;

let build_cc cfg = { itemsets = Map.empty; gotos = Map.empty }
;;

let build_tables cfg cc = (Map.empty, Map.empty)
;;

let simulate cfg acts gotos lexemes = Parse.Foo
;;

let generate cfg lexemes = Parse.Bar
;;
