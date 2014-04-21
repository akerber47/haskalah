open Batteries
;;
open Types
;;

(* Add the given entry to the map, unless that key is already present, in which
 * case indicate the conflict by storing None for that key.
 * ('a, 'b option) Map.t -> 'a -> 'b -> ('a, 'b option) Map.t *)
let map_add_conflict m x y =
  if Map.mem x m then
    Map.add x None m
  else
    Map.add x (Some y) m
;;

(* Look up the given prename in the given environment. Error if ambiguous.
 * None if not present.
 * environment -> prename -> name option *)
let lookup env pn =
  if Map.mem pn env then
    match Map.find pn env with
    | Some n -> Some n
    | None -> raise (Name_error begin
      match pn with
      | (_,Some pfx,s) ->
        Printf.sprintf2
          "Identifier '%s.%s' is ambiguous\n" pfx s
      | (_,_,s) ->
        Printf.sprintf2
          "Identifier '%s' is ambiguous\n" s
    end)
  else
    None
;;

(* ast1 <Ast1_*leaf> -> string *)
let leaf_contents ast =
  match ast with
  | Ast1_parenthesized_leaf l -> l.contents
  | Ast1_backquoted_leaf l -> l.contents
  | Ast1_leaf l -> l.contents
  | _ -> assert false
;;

(* Return a list of the identifiers bound in a given pattern expression.
 * ast1 <Ast1_*pat*_*> -> string list *)
let rec pat_get_binds ast =
  match ast.node with
  (* Basically, just map it all down to the lowest-level patterns (apats) *)
  | Ast1_pat a1 -> pat_get_binds a1
  | Ast1_infixpat_op (a1,_,a3) -> (pat_get_binds a1) @ (pat_get_binds a3)
  | Ast1_infixpat_pat10 a1 -> pat_get_binds a1
  | Ast1_pat10_con (_,a2s) -> List.concat (List.map pat_get_binds a2s)
  | Ast1_pat10_apat a1 -> pat_get_binds a1
  (* pattern "v" binds v *)
  | Ast1_apat_var nv -> [leaf_contents nv]
  (* pattern "v@(...)" binds v *)
  | Ast1_apat_as (nv,a2) -> (leaf_contents nv)::(pat_get_binds a2)
  (* All other cases either bind nothing or just traverse down recursively *)
  | Ast1_apat_con _
  | Ast1_apat_literal _
  | Ast1_apat_wild -> []
  | Ast1_apat_lbpat (_, a2s) -> List.concat (List.map pat_get_binds a2s)
  | Ast1_apat_paren a1 -> pat_get_binds a1
  | Ast1_apat_tuple a1s -> List.concat (List.map pat_get_binds a1s)
  | Ast1_apat_list a1s -> List.concat (List.map pat_get_binds a1s)
  | Ast1_apat_irref a1 -> pat_get_binds a1
  | Ast1_fpat (a1,a2) -> pat_get_binds a2
;;


(* Return a list of identifiers bound in function or pattern bindings in the
 * given declaration. Empty list if not a function or pattern binding.
 * ast1 <Ast1_decl_*> -> string list *)
let decl_get_binds ast =
  match ast.node with
  | Ast1_decl_funbind (funlhs_ast, _) ->
      let rec do_funlhs fl_ast =
        match fl_ast.node with
        (* "f x y = ..." declares f *)
        | Ast1_funlhs_fun (nf, _) -> [leaf_contents nf]
        (* "x `op` y = ..." declares op *)
        | Ast1_funlhs_funop (_, nop, _) -> [leaf_contents nop]
        (* (f x y) z = ..." declares f *)
        | Ast1_funlhs_nested (nsubfl, _) -> do_funlhs nsubfl
      in
      do_funlhs funlhs_ast
  | Ast1_decl_patbind (infixpat_ast, _) ->
      pat_get_binds infixpat_ast
  | Ast1_decl_type _
  | Ast1_decl_fixity _
  | Ast1_decl_empty -> []
  | _ -> assert false
;;

let build_globals ast =
  (* Look up name of module currently being compiled. *)
  let curmod =
    match ast.node with
    | Ast1_module (Some m, _, _) -> leaf_contents m
    | _ -> "Main" (* Default for unnamed modules *)
  in
  (* Return a list of all the top-level identifiers bound by the given
   * top-level declaration, along with namespace and srcmod info to feed
   * into add_names.
   * ast1 <node=Ast1_topdecl_*> -> (namespace * string * string) list *)
  let get_topdecl_names ast =
    match ast.node with
    (* "type T a b = t" declares T *)
    | Ast1_topdecl_type (
      { node = Ast1_simpletype (
        { node = nT; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod)]
    (* "newtype T a b = N t" declares T and N *)
    | Ast1_topdecl_newtype (
      { node = Ast1_simpletype (
        { node = nT; _},_);_},
      { node = Ast1_newconstr_con (
        { node = nN; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod);
           (Ns_data, leaf_contents nN, curmod)]
    (* "newtype T a b = N { f :: t }" declares T, N, and f *)
    | Ast1_topdecl_newtype (
      { node = Ast1_simpletype (
        { node = nT; _},_);_},
      { node = Ast1_newconstr_field (
        { node = nN; _},
        { node = nf; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod);
           (Ns_data, leaf_contents nN, curmod);
           (Ns_var, leaf_contents nf, curmod)]
    (* Similarly for data declarations, except multiple constructors (and
     * constructors can now be operators)
     * "data T a b = N1 t | N2 t | ..." declares T (at least) *)
    | Ast1_topdecl_data (
      { node = Ast1_simpletype (
        { node = nT; _},_);_},
      constrs,_) ->
        let do_constr c_ast =
          match c_ast.node with
          (* constructor "N1 t1 t2 t3" declares N1 *)
          | Ast1_constr_con ({ node = nN1; _},_) ->
              [(Ns_data, leaf_contents nN1, curmod)]
          (* constructor "t1 :n2 t2" declares :n2 *)
          | Ast1_constr_conop (_, { node = nn2; _},_) ->
              [(Ns_data, leaf_contents nn2, curmod)]
          (* constructor "N3 { f1,f2 :: t1, f3 :: t2, ... }" declares
           * N3 and f1,f2,f3 *)
          | Ast1_constr_fields ({ node = nN3; _}, fd_asts) ->
              let do_fielddecl fd_ast =
                match fd_ast.node with
                | Ast1_fielddecl (nf1s,_) ->
                    List.map (fun nf1 -> (Ns_var, leaf_contents nf1, curmod))
                      nf1s
                | _ -> assert false
              in
              (Ns_data, leaf_contents nN3, curmod)::
                (List.concat (List.map do_fielddecl fd_asts))
          | _ -> assert false
        in
        (Ns_tc, leaf_contents nT, curmod)::
          (List.concat (List.map do_constr constrs))
    (* "class ... => C a where ..." declares C (at least) *)
    | Ast1_topdecl_class (_, { node = nC; _}, _, cdecls) ->
        let do_cdecl c_ast =
          match c_ast.node with
          (* class method type decl "op1,op2 :: t" declares op1 and op2 *)
          | Ast1_decl_type (nop1s, _, _) ->
              List.map (fun nop1 -> (Ns_var, leaf_contents nop1, curmod)) nop1s
          (* No other declarations in a class affect global namespace *)
          | _ -> []
        in
        (Ns_class, leaf_contents nC, curmod)::
          (List.concat (List.map do_cdecl cdecls))
    (* instance and default declarations do not affect global namespace *)
    | Ast1_topdecl_instance _ -> []
    | Ast1_topdecl_default _ -> []
    (* Finally, any other top-level declarations work just like inner
     * declarations (except that they're at top level. *)
    | Ast1_topdecl_decl { node = d; _} ->
        List.map (fun id -> (Ns_var, id, curmod))
          (List.concat (decl_get_binds d))
    | _ -> assert false
  (* Given some info about a module's imported names, add those names to the
   * global environment. Add name conflicts where appropriate. Note that we
   * don't bother with helpful error messages for name conflicts.
   * The info is:
   * - prefix to add these names to globals under (the "as" module)
   * - bool for whether or not to add the corresp unqualified names
   * - triples (namespace, rawname, real_source_module) of things to add
   *   (note that real source module for a name might require a bunch of nested
   *   imports to dig up, but this is taken care of in get_import_names)
   * globals -> string * bool * (namespace * string * string) list -> globals
   * *)
  and add_names glbenv (pfx, add_unq, names) =
    List.fold_left
      (fun g (ns, rn, srcmod) ->
        if add_unq then
          map_add_conflict
            (map_add_conflict g (ns, Some pfx, rn)
                                (Name_global (pn,srcmod)))
            (ns, None, rn)
            (Name_global (pn,srcmod))
        else
          map_add_conflict g (ns, Some pfx, rn)
                             (Name_global (pn,srcmod))
      glbenv
      names
  in
  match ast.node with
  | Ast1_module (_, _, { node = Ast1_body topdecls; _ }) ->
      let mynames = List.concat (List.map get_topdecl_names topdecls)
      and imported_names = Manager.get_import_names ast
      List.fold_left add_names
        Map.empty
        ((cm,true,mynames)::imported_names)
  | _ -> assert false
;;

