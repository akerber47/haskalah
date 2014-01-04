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
  num_itemsets : int;
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

(* Return an array of all possible right-hand sides of productions of the given
 * nonterminal. *)
let getrhss cfg nt =
  Array.map (fun p -> p.rhs)
            (Array.filter (fun p -> p.lhs = nt) cfg.productions)
;;

let first_sets cfg =
  let curmap = ref Map.empty
  and all_nonterms =
      List.unique (Array.to_list (Array.map (fun p -> p.lhs) cfg.productions))
  in let rec do_nonterm nextnt =
    if Map.mem nextnt !curmap then
      ()
    else begin
      (* Get all possible RHS productions *)
      let rhss = getrhss cfg nextnt
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

(* Compute the "nonterminal expansion closure" of an itemset. Basically, if
  * we're given a set of items we're in a state to receive, this computes
  * other items we should also be willing to receive in this same state (by
  * repeatedly expanding the rhs of our given items after the dot). *)
let rec closure cfg itmst =
  let fstable = first_sets cfg
  and still_adding = ref false
  and new_itmst = ref itmst in
  let do_item itm =
    let rhs = cfg.productions.(itm.prod).rhs in
    (* If any item's dot is directly in front of a nonterminal, need to
     * expand that nonterminal (to compute closure - ie all items that
     * "could match this dot position") *)
    if itm.dot < List.length rhs then
      match List.drop itm.dot rhs with
      | (Nonterminal nt)::rhsrest ->
        (* Add new items for that nonterminal expansion to the new itemset -
         * these are possible handles for the current position of our dot. We
         * just need to compute what the lookahead symbols after this
         * expansion can be. *)
        List.iter
          (fun prod_i ->
            (* Possible lookahead symbols for new expansion are things in
             * the first set of "stuff after where we expanded" - ie the rest
             * of the original rhs and the original lookahead symbol. Note
             * that we know this first set can't contain epsilon because the
             * string ends in a terminal. *)
            TermSet.iter
              (fun t -> let new_item = { prod = prod_i;
                                         dot = 0;
                                         lookahead = t; } in
                        (* Keep track of whether we added anything. *)
                        if not (ItemSet.mem new_item !new_itmst) then begin
                          new_itmst := ItemSet.add new_item !new_itmst;
                          still_adding := true
                        end)
              first_set_string fstable (rhsrest @ [itm.lookahead]).terms)
          (Util.findi_all (fun prod -> prod.lhs = nt) cfg.productions)
      | _ -> ()
  in begin
    ItemSet.iter do_item itmst;
    if !still_adding then
      closure !new_itmst
    else
      itmst
  end
;;

(* Compute the "goto" of an item set on a terminal. This computes the
 * possible items we'll be ready to receive after being in a state to
 * accept the given set of items and reading in the given terminal. Note that
 * if the given terminal doesn't match *any* of "next desired terminals"
 * (terminals immediately following the dot) of the given items, we will get
 * an empty set. *)
let goto cfg itmst t =
  let next_itmst = ref itmst in begin
  (* Look for all items which explicitly have that terminal immediately
   * following the dot in their rhs. Then move the dot. *)
    ItemSet.iter
      (fun itm ->
        let rhs = cfg.productions.(itm.prod).rhs in
          if itm.dot < List.length rhs &&
              List.nth itm.dot rhs = (Terminal t) then
            next_itmset := ItemSet.add {itm with dot=itm.dot+1} !next_itmst)
      itmst;
    (* Finally, compute the closure of all those items post dot transition *)
    closure !next_itmst
  end
;;

let build_cc cfg =
  (* Keep track of the item sets and gotos we've found so far. *)
  let cur_itemsets = ref Map.empty
  and cur_gotos = ref Map.empty
  (* Keep track of what itemsets we've processed, as well as what index we're
   * going to give the next itemset we add. *)
  and ix_first_unprocessed = ref 0
  and ix_new_itemset = ref 1
  (* Small helper function: tries to find an item among itemsets we've already
   * added. Returns Some int (the index of the itemset) if found, None if this
   * item is new. *)
  in let lookup_item itm =
    let rec do_ix i =
      if i < !ix_new_itemset then
        if ItemSet.mem itm (Map.find i !cur_itemsets) then
          Some i
        else
          do_ix (i+1)
      else
        None
    in do_ix 0
  in
  begin
    (* Start by adding the 0th itemset, corresponding to the start state. *)
    let itmst_zero = closure cfg (List.fold_left
      (* In the start state, we want to match the goal symbol somehow
       * (prod has lhs = goal), we haven't matched anything yet (dot = 0), and
       * the next thing we encounter after that should be EOF. *)
      (fun itmst prod_i ->
        ItemSet.add { prod = prod_i; dot = 0; lookahead = Lex.EOF } itmst)
      ItemSet.empty
      (Util.findi_all (fun prod -> prod.lhs = cfg.goal) cfg.productions))
    in cur_itemsets := Map.add 0 itmst_zero !cur_itemsets;
    (* Now, repeatedly iterate over all unprocessed itemsets until there are no
     * longer any new itemsets being added. *)
    while !ix_first_unprocessed < !ix_new_itemset do
      for ix = !ix_first_unprocessed to (!ix_new_itemset - 1) do begin
        let itmst = Map.find ix !cur_itemsets in
        (* Look for terminals following dots among items in itmst. These will
         * be the only possibilities that make (goto tmst t) nonempty. *)
        let next_gotos = ItemSet.fold_left
          (fun tset itm ->
            let rhs = cfg.productions.(itm.prod).rhs in
              if itm.dot < List.length rhs &&
                  List.nth itm.dot rhs = (Terminal t) then
                TermSet.add t ts
              else
                ts)
          TermSet.empty
          itmst
        (* Check whether goto(itmst,t) yields a *new* itemset, for each t, and
         * if so add it to the itemset table. Either way, add this transition
         * to the goto table. *)
        in
        TermSet.iter
          (fun t ->
            let next_itmst = goto cfg itmst t in
            (* Since all itemsets are closed, to check if we've already seen
             * this itemset it's enough to look up one representative. *)
            let next_itmst_repr = ItemSet.choose next_itmst in
            let next_ix = match lookup_item next_itmset_repr with
            | (Some existing_ix) -> existing_ix
            | None -> begin
              cur_itemsets := Map.add !ix_new_itemset next_itmst !cur_itemsets;
              ix_new_itemset := !ix_new_itemset + 1;
              !ix_new_itemset - 1
            end
            in
            Map.add (ix, t) next_ix !cur_gotos)
          next_gotos;
        ix_first_unprocessed := !ix_first_unprocessed + 1
      end done
    done;
    { itemsets = !cur_itemsets;
      gotos = !cur_gotos;
      num_itemsets = !ix_new_itemset; }
  end
;;

let build_tables cfg cc = (Map.empty, Map.empty)
;;

let simulate cfg acts gotos lexemes = Parse.Foo
;;

let generate cfg lexemes = Parse.Bar
;;
