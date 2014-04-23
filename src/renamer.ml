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

(* Remove consecutive duplicates from a list, with the caveat that only
 * duplicated elements where all duplicates have a true flag can be removed in
 * this way. Return the resulting list without those flags.
 * ('a -> 'a  -> bool) -> ('a * bool) list -> 'a list *)
let uniq_consecutive_flag eq lst =
  let (newlst, _) = List.fold_left
    (fun (acc,lastflagged) (x,flag) ->
      if flag then
        match lastflagged with
        (* If we match last flagged, drop this duplicate from list *)
        | Some y when eq x y -> (acc,lastflagged)
        (* Otherwise, this is the new last flagged, keep in list *)
        | Some _
        | None -> (acc @ [x], Some x)
      else
        (* Otherwise, there is no last flagged, keep in list *)
        (acc @ [x], None))
    ([],None)
    lst
  in
  newlst
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
  match ast.node1 with
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
 * Also, return true if function binding, false otherwise.
 * ast1 <Ast1_decl_*> -> string list * bool *)
let decl_get_binds ast =
  match ast.node1 with
  | Ast1_decl_funbind (funlhs_ast, _) ->
      let rec do_funlhs fl_ast =
        match fl_ast.node1 with
        (* "f x y = ..." declares f *)
        | Ast1_funlhs_fun (nf, _) -> [leaf_contents nf]
        (* "x `op` y = ..." declares op *)
        | Ast1_funlhs_funop (_, nop, _) -> [leaf_contents nop]
        (* (f x y) z = ..." declares f *)
        | Ast1_funlhs_nested (nsubfl, _) -> do_funlhs nsubfl
      in
      (do_funlhs funlhs_ast, true)
  | Ast1_decl_patbind (infixpat_ast, _) ->
      (pat_get_binds infixpat_ast, false)
  | Ast1_decl_type _
  | Ast1_decl_fixity _
  | Ast1_decl_empty -> ([], false)
  | _ -> assert false
;;

(* Takes in a list of declarations, and returns list of identifiers, similarly
 * to above. Combines multi-line function bindings into single declared id.
 * ast1 <Ast1_decl_*> list -> string list *)
let decls_get_binds asts =
  List.concat (uniq_consecutive_flag (=)
    (List.map decl_get_binds asts))
;;

let build_globals ast =
  (* Look up name of module currently being compiled. *)
  let curmod =
    match ast.node1 with
    | Ast1_module (Some m, _, _) -> leaf_contents m
    | _ -> "Main" (* Default for unnamed modules *)
  in
  (* Return a list of all the top-level identifiers bound by the given
   * top-level declaration, along with namespace and srcmod info to feed
   * into add_names. Also, return true if function binding, false otherwise.
   * This is so we can combine multi-line function declaration into one
   * declared identifier.
   * ast1 <node=Ast1_topdecl_*> -> (namespace * string * string) list * bool *)
  let get_topdecl_names ast =
    match ast.node1 with
    (* "type T a b = t" declares T *)
    | Ast1_topdecl_type (
      { node1 = Ast1_simpletype (
        { node1 = nT; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod)], false
    (* "newtype T a b = N t" declares T and N *)
    | Ast1_topdecl_newtype (
      { node1 = Ast1_simpletype (
        { node1 = nT; _},_);_},
      { node1 = Ast1_newconstr_con (
        { node1 = nN; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod);
           (Ns_data, leaf_contents nN, curmod)], false
    (* "newtype T a b = N { f :: t }" declares T, N, and f *)
    | Ast1_topdecl_newtype (
      { node1 = Ast1_simpletype (
        { node1 = nT; _},_);_},
      { node1 = Ast1_newconstr_field (
        { node1 = nN; _},
        { node1 = nf; _},_);_},_) ->
          [(Ns_tc, leaf_contents nT, curmod);
           (Ns_data, leaf_contents nN, curmod);
           (Ns_var, leaf_contents nf, curmod)], false
    (* Similarly for data declarations, except multiple constructors (and
     * constructors can now be operators)
     * "data T a b = N1 t | N2 t | ..." declares T (at least) *)
    | Ast1_topdecl_data (
      { node1 = Ast1_simpletype (
        { node1 = nT; _},_);_},
      constrs,_) ->
        let do_constr c_ast =
          match c_ast.node1 with
          (* constructor "N1 t1 t2 t3" declares N1 *)
          | Ast1_constr_con ({ node1 = nN1; _},_) ->
              [(Ns_data, leaf_contents nN1, curmod)]
          (* constructor "t1 :n2 t2" declares :n2 *)
          | Ast1_constr_conop (_, { node1 = nn2; _},_) ->
              [(Ns_data, leaf_contents nn2, curmod)]
          (* constructor "N3 { f1,f2 :: t1, f3 :: t2, ... }" declares
           * N3 and f1,f2,f3 *)
          | Ast1_constr_fields ({ node1 = nN3; _}, fd_asts) ->
              let do_fielddecl fd_ast =
                match fd_ast.node1 with
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
          (List.concat (List.map do_constr constrs)), false
    (* "class ... => C a where ..." declares C (at least) *)
    | Ast1_topdecl_class (_, { node1 = nC; _}, _, cdecls) ->
        let do_cdecl c_ast =
          match c_ast.node1 with
          (* class method type decl "op1,op2 :: t" declares op1 and op2 *)
          | Ast1_decl_type (nop1s, _, _) ->
              List.map (fun nop1 -> (Ns_var, leaf_contents nop1, curmod)) nop1s
          (* No other declarations in a class affect global namespace *)
          | _ -> []
        in
        (Ns_class, leaf_contents nC, curmod)::
          (List.concat (List.map do_cdecl cdecls)), false
    (* instance and default declarations do not affect global namespace *)
    | Ast1_topdecl_instance _
    | Ast1_topdecl_default _ -> [], false
    (* import declarations taken care of by compilation manager *)
    | Ast1_topdecl_import _ -> [], false
    (* Finally, any other top-level declarations work just like inner
     * declarations (except that they're at top level. *)
    | Ast1_topdecl_decl { node1 = d; _} ->
        let (binds,isfun) = decl_get_binds d
        in
        (List.map (fun id -> (Ns_var, id, curmod)) binds), isfun
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
  (* Check that all the global names defined by the given triples are unique.
   * Raise error on any duplicates.
   * (namespace * string * string) list -> unit *)
  and check_unique lst =
    List.iter
      (fun gp ->
        if List.length gp > 1 then
          let (_, id, _) = List.hd gp in
          raise (Name_error (Printf.sprintf2
            "Identifier '%s' already defined" id)))
      (List.group (=) lst)
  in
  match ast.node1 with
  | Ast1_module (_, _, { node1 = Ast1_body topdecls; _ }) ->
      (* Get all names from top-level declarations in current file, combining
       * adjacent function bindings. *)
      let mynames = List.concat (uniq_consecutive_flag (=)
        (List.map get_topdecl_names topdecls))
      and imported_names = Manager.get_import_names ast
      in begin
        check_unique mynames;
        List.fold_left add_names
          Map.empty
          ((cm,true,mynames_dedup)::imported_names)
      end
  | _ -> assert false
;;

(* Create a new local name for the given identifier in the given namespace,
 * and add the corresponding binding to the given name environment.
 * Uses shared state to create unique disambiguator for given local name.
 * namespace -> string -> enivronment -> environment *)
let add_local ns id env =
  let u = Unique.get id in
  Map.add (ns, None, id) (Name_local (ns, id, u)) env
;;

let remove_local ns id env =
  Map.remove (ns, None, id) env
;;

(* Same thing, but with a bunch of local names, and checking to make sure there
 * are no duplicates. *)
let add_locals_uniq ns ids env = begin
  (* Check for duplicates... *)
  List.iter
    (fun gp ->
      if List.length gp > 1 then
        raise (Name_error (Printf.sprintf2
          "Identifier '%s' already defined" List.hd gp)))
    (List.group (=) ids);
  (* And add all to env *)
  List.fold_left (fun e id -> add_local ns id e) env ids
end
;;

(* Take in an ast <Ast1_simpletype, Ast1_inst_*, or Ast1_*type_*> and build a
 * list of all the (lowercase) type variable identifiers used in it.
 * ast1 <Ast1_*type*> -> string list *)
let rec get_tyvars ast =
  match ast.node1 with
  | Ast1_simpletype (_,a2s) ->
      List.map leaf_contents a2s
  | Ast1_inst_con _ ->
      []
  | Ast1_inst_app (a1,a2s) ->
      List.map leaf_contents a2s
  | Ast1_inst_tuple a1s ->
      List.map leaf_contents a2s
  | Ast1_inst_list a1 ->
      [leaf_contents a1]
  | Ast1_inst_fun (a1,a2) ->
      [leaf_contents a1; leaf_contents a2]
  | Ast1_type_fun (a1,a2) ->
      (get_tyvars a1) @ (get_tyvars a2)
  | Ast1_type_btype a1 ->
      get_tyvars a1
  | Ast1_btype_app (a1,a2) ->
      (get_tyvars a1) @ (get_tyvars a2)
  | Ast1_btype_atype a1 ->
      get_tyvars a1
  | Ast1_atype_con _ ->
      []
  | Ast1_atype_var a1 ->
      [leaf_contents a1]
  | Ast1_atype_tuple a1s ->
      List.concat (List.map get_tyvars a1s)
  | Ast1_atype_list a1 ->
      get_tyvars a1
  | Ast1_atype_paren a1 ->
      get_tyvars a1
  | _ -> assert false


(* Basically just recursively descend through the tree, keeping track of the
 * current environment at all times. When we reach a leaf, use the local
 * namespace and current environment to look up that lexeme (if it's an
 * identifier) *)
let rec rename env ast =
  let newnode1 = match ast.node1 with
  | Ast1_module (oa1, oa2s, a3) ->
      Ast1_module (oa1, Option.map (List.map (rename env)) oa2s, rename env a3)
  | Ast1_body a1s ->
      Ast1_body (List.map rename env a1s)
  (* Rename exports using global namespace *)
  | Ast1_export_var a1 ->
      Ast1_export_var (rename env a1)
  | Ast1_export_type (a1, oa2s) ->
      Ast1_export_type (rename env a1, Option.map (List.map rename env) oa2s)
  | Ast1_export_module a1 ->
      Ast1_export_module (rename env a1)
  (* Don't bother to rename imports (in fact, we never touch import statements
   * again). *)
  | Ast1_topdecl_import (oa1, a2, oa34, oa5) ->
      Ast1_topdecl_import (oa1, a2, oa34, oa5)
  | Ast1_impspec _
  | Ast1_import_var _
  | Ast1_import_type _ -> assert false
  (* From here down, we actually have to add local names to the environment. *)
  (* In "type T a b = ..." only a and b are allowed in rhs. Similarly for data
   * and newtype declarations. *)
  | Ast1_topdecl_type (lhs, rhs) ->
      let newenv = add_locals_uniq Ns_tv (get_tyvars lhs) env in
      Ast1_topdecl_type (rename newenv lhs, rename newenv rhs)
  | Ast1_topdecl_data (lhs, rhss, der) ->
      let newenv = add_locals_uniq Ns_tv (get_tyvars lhs) env in
      Ast1_topdecl_data (rename newenv lhs, List.map (rename newenv) rhss, oa3)
  | Ast1_topdecl_newtype (lhs, rhs, der) ->
      let newenv = add_locals_uniq Ns_tv (get_tyvars lhs) env in
      Ast1_topdecl_data (rename newenv lhs, rename newenv rhs, oa3)
  (* In "class (B a, ...) => C a where ...", only a is allowed in context, and
   * is a bound (non-polymorphic) type variable in declaration body. *)
  | Ast1_topdecl_class (ctxt, nC, na, body) ->
      (* string -> environment -> ast <Ast1_decl_*> -> ast <Ast1_decl_*> *)
      let do_cdecl id e ast =
        match ast.node1 with
        (* Process any type signatures like normal, but make sure to bind the
         * given id to the correct tv name (given at top of class decl).
         * We do this by making sure to *not* add an inner local type variable
         * for it. *)
        | Ast1_decl_type (vars, ctxt, t) ->
            let t_locals =
              let t_orig_locals = List.unique (get_tyvars t)) in begin
                (* Check that we actually use the class type variable *)
                if not (List.mem id t_orig_locals) then
                  raise (Name_error (Printf.sprintf2
                    "Class type variable '%s' unused in method signature" id));
                List.remove t_orig_locals id
              end
            in
            let t_e = add_locals_uniq Ns_tv t_locals e in
            (* Cannot constrain class type variable in its method's context.
             * Yes, maybe we could produce a useful error message in this case.
             * Too much work. *)
            let ctxt_e = remove_local Ns_tv id e in
            Ast1_decl_type (List.map (rename e) vars,
              Option.map (rename ctxt_e) ctxt, rename t_e t)
        | Ast1_decl_funbind _
        | Ast1_decl_patbind _
        | Ast1_decl_fixity _
        | Ast1_decl_empty -> rename e ast
        | _ -> assert false
      let newenv = add_local Ns_tv (leaf_contents v) env in
      Ast1_topdecl_class (Option.map (rename newenv) ctxt, rename env nC,
        rename newenv na, List.map (do_cdecl (leaf_contents v) newenv) body)
  (* In "instance (B a, ...) => C (T a b ...) where ...", only a, b, ...
   * allowed in context declaration. No type signatures allowed in body, so no
   * need to worry about that. *)
  | Ast1_topdecl_instance (ctxt, c, inst, body) ->
      let newenv = add_locals_uniq Ns_tv (get_tyvars inst) env in
      Ast1_topdecl_instance (Option.map (rename newenv) ctxt, rename env c,
        rename newenv inst, List.map (rename env) body)
  | Ast1_topdecl_default a1s ->
      Ast1_topdecl_default (List.map (rename env) a1s)
  | Ast1_topdecl_decl a1 ->
      Ast1_topdecl_decl (rename env a1)
  (* TODO *)
  | Ast1_decl_bind (a1,a2) ->
      Ast1_decl_bind (rename env a1,rename env a2)
  | Ast1_decl_type (a1s,a2) ->
      Ast1_decl_type (List.map rename env a1s,rename env a2)
  | Ast1_decl_fixity (a1,oa2,a3s) ->
      Ast1_decl_fixity (rename env a1,Option.map rename env oa2,List.map rename env a3s)
  | Ast1_decl_empty ->
      Ast1_decl_empty
  | Ast1_type_context (a1,a2) ->
      Ast1_type_context (rename env a1,rename env a2)
  | Ast1_type_fun (a1,a2) ->
      Ast1_type_fun (rename env a1,rename env a2)
  | Ast1_type_btype a1 ->
      Ast1_type_btype (rename env a1)
  | Ast1_btype_app (a1,a2) ->
      Ast1_btype_app (rename env a1,rename env a2)
  | Ast1_btype_atype a1 ->
      Ast1_btype_atype (rename env a1)
  | Ast1_atype_con a1 ->
      Ast1_atype_con (rename env a1)
  | Ast1_atype_var a1 ->
      Ast1_atype_var (rename env a1)
  | Ast1_atype_tuple a1s ->
      Ast1_atype_tuple (List.map rename env a1s)
  | Ast1_atype_list a1 ->
      Ast1_atype_list (rename env a1)
  | Ast1_atype_paren a1 ->
      Ast1_atype_paren (rename env a1)
  | Ast1_gtycon_con a1 ->
      Ast1_gtycon_con (rename env a1)
  | Ast1_gtycon_unit ->
      Ast1_gtycon_unit
  | Ast1_gtycon_list ->
      Ast1_gtycon_unit
  | Ast1_gtycon_fun ->
      Ast1_gtycon_fun
  | Ast1_gtycon_tuple a1s ->
      Ast1_gtycon_tuple (List.map rename env a1s)
  | Ast1_scontext a1s ->
      Ast1_scontext (List.map rename env a1s)
  | Ast1_simpleclass (a1,a2) ->
      Ast1_simpleclass (rename env a1,rename env a2)
  | Ast1_simpletype (a1,a2s) ->
      Ast1_simpletype (rename env a1,List.map rename env a2s)
  | Ast1_constr_con a1 ->
      Ast1_constr_con (rename env a1)
  | Ast1_constr_conop (a1,a2,a3) ->
      Ast1_constr_conop (rename env a1,rename env a2,rename env a3)
  | Ast1_constr_fields (a1,a2s) ->
      Ast1_constr_fields (rename env a1,List.map rename env a2s)
  | Ast1_newconstr_con (a1,a2) ->
      Ast1_newconstr_con (rename env a1,rename env a2)
  | Ast1_newconstr_field (a1,a2,a3) ->
      Ast1_newconstr_field (rename env a1,rename env a2,rename env a3)
  | Ast1_fielddecl (a1s,a2) ->
      Ast1_fielddecl (List.map rename env a1s,rename env a2)
  | Ast1_deriving a1s ->
      Ast1_deriving (List.map rename env a1s)
  | Ast1_inst_con a1 ->
      Ast1_inst_con (rename env a1)
  | Ast1_inst_app (a1,a2s) ->
      Ast1_inst_app (rename env a1,List.map rename env a2s)
  | Ast1_inst_tuple a1s ->
      Ast1_inst_tuple (List.map rename env a1s)
  | Ast1_inst_list a1 ->
      Ast1_inst_list (rename env a1)
  | Ast1_inst_fun (a1,a2) ->
      Ast1_inst_fun (rename env a1,rename env a2)
  | Ast1_rhs_eq (a1,oa2s) ->
      Ast1_rhs_eq (rename env a1,Option.map (List.map rename env) oa2s)
  | Ast1_rhs_guard (a1s,oa2s) ->
      Ast1_rhs_guard (List.map rename env a1s,Option.map (List.map rename env) oa2s)
  | Ast1_gdrhs (a1,a2) ->
      Ast1_gdrhs (rename env a1,rename env a2)
  | Ast1_exp (a1,oa2) ->
      Ast1_exp (rename env a1,Option.map rename env oa2)
  | Ast1_infixexp_op (a1,a2,a3) ->
      Ast1_infixexp_op (rename env a1,rename env a2,rename env a3)
  | Ast1_infixexp_exp10 a1 ->
      Ast1_infixexp_exp10 (rename env a1)
  | Ast1_exp10_lambda (a1s,a2) ->
      Ast1_exp10_lambda (List.map rename env a1s,rename env a2)
  | Ast1_exp10_let (a1s,a2) ->
      Ast1_exp10_let (List.map rename env a1s,rename env a2)
  | Ast1_exp10_if (a1,a2,a3) ->
      Ast1_exp10_if (rename env a1,rename env a2,rename env a3)
  | Ast1_exp10_case (a1,a2s) ->
      Ast1_exp10_case (rename env a1,List.map rename env a2s)
  | Ast1_exp10_do a1s ->
      Ast1_exp10_do (List.map rename env a1s)
  | Ast1_exp10_aexps a1s ->
      Ast1_exp10_aexps (List.map rename env a1s)
  | Ast1_aexp_var a1 ->
      Ast1_aexp_var (rename env a1)
  | Ast1_aexp_con a1 ->
      Ast1_aexp_con (rename env a1)
  | Ast1_aexp_literal a1 ->
      Ast1_aexp_literal (rename env a1)
  | Ast1_aexp_paren a1 ->
      Ast1_aexp_paren (rename env a1)
  | Ast1_aexp_tuple a1s ->
      Ast1_aexp_tuple (List.map rename env a1s)
  | Ast1_aexp_list a1s ->
      Ast1_aexp_list (List.map rename env a1s)
  | Ast1_aexp_seq (a1, oa2, oa3) ->
      Ast1_aexp_seq (rename env a1, Option.map rename env oa2, Option.map rename env oa3)
  | Ast1_aexp_comp (a1,a2s) ->
      Ast1_aexp_comp (rename env a1,List.map rename env a2s)
  | Ast1_aexp_lsec (a1,a2) ->
      Ast1_aexp_lsec (rename env a1,rename env a2)
  | Ast1_aexp_rsec (a1,a2) ->
      Ast1_aexp_rsec (rename env a1,rename env a2)
  | Ast1_aexp_lbupdate (a1,a2s) ->
      Ast1_aexp_lbupdate (rename env a1,List.map rename env a2s)
  | Ast1_aexp_aspat (a1,a2) ->
      Ast1_aexp_aspat (rename env a1,rename env a2)
  | Ast1_aexp_irrefpat a1 ->
      Ast1_aexp_irrefpat (rename env a1)
  | Ast1_aexp_wildpat ->
      Ast1_aexp_wildpat
  | Ast1_qual_assign (a1,a2) ->
      Ast1_qual_assign (rename env a1,rename env a2)
  | Ast1_qual_let a1s ->
      Ast1_qual_let (List.map rename env a1s)
  | Ast1_qual_guard a1 ->
      Ast1_qual_guard (rename env a1)
  | Ast1_alt_match (a1,a2,oa3s) ->
      Ast1_alt_match (rename env a1,rename env a2,Option.map (List.map rename env) oa3s)
  | Ast1_alt_guard (a1,a2s,oa3s) ->
      Ast1_alt_guard (rename env a1,List.map rename env a2s,Option.map (List.map rename env) oa3s)
  | Ast1_gdpat (a1,a2) ->
      Ast1_gdpat (rename env a1,rename env a2)
  | Ast1_stmt_exp a1 ->
      Ast1_stmt_exp (rename env a1)
  | Ast1_stmt_assign (a1,a2) ->
      Ast1_stmt_assign (rename env a1,rename env a2)
  | Ast1_stmt_let a1s ->
      Ast1_stmt_let (List.map rename env a1s)
  | Ast1_stmt_empty ->
      (Ast1_stmt_empty)
  | Ast1_fbind (a1, a2) ->
      Ast1_fbind (rename env a1, rename env a2)
  | Ast1_gcon_unit ->
      Ast1_gcon_unit
  | Ast1_gcon_list ->
      Ast1_gcon_list
  | Ast1_gcon_tuple a1s ->
      Ast1_gcon_tuple (List.map rename env a1s)
  | Ast1_gcon_qcon a1 ->
      Ast1_gcon_qcon (rename env a1)
  | Ast1_parenthesized_leaf l ->
      Ast1_parenthesized_leaf l
  | Ast1_backquoted_leaf l ->
      Ast1_backquoted_leaf l
  | Ast1_leaf l ->
      Ast1_leaf l
  in { node = newnode; blockstart = ast.blockstart; blockend = ast.blockend }
