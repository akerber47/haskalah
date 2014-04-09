open Batteries
;;
open Types
;;

(* Take in ast0 and ast1node, copies the bounds over. *)
let do_bound a0 a1node =
  { node1 = a1node; blockstart1 = a0.blockstart; blockend1 = a0.blockend }
;;

(* Take in an ast Ast0_type_*, and verify that it has either a valid context or no
 * context at all. Return ( ast Ast1_context option, ast Ast1_type_* ) *)
let rec check_context ast =
  (* Take in an Ast0_btype_*, and verify that it *is* a valid context. Convert
   * it to an Ast1_context, error otherwise. *)
  let rec check_iscontext ast =
    let newnode = match ast.node with
    (* Application of class constructor - context can only consist of a single
     * class. *)
    | Ast0_btype_app _ ->
        Ast1_context [check_isclass ast]
    | Ast0_btype_atype a1 ->
        match a1.node with
        (* Tuple type - convert to multi-class context *)
        | Ast0_atype_tuple a1s ->
            Ast1_context (List.map check_typeisclass a1s)
        (* Paren type - convert to single-class context *)
        | Ast0_atype_paren a1 ->
            Ast1_context [check_typeisclass a1]
        (* Otherwise, cannot appear as context *)
        | _ ->
          raise (Parse_error (Printf.sprintf2
            "Illegal context: at %d-%d\n"
            ast.blockstart ast.blockend))
    | _ -> assert false
    in do_bound ast newnode
  (* Take in an Ast0_btype_*, and verify that it *is* a valid class constraint
   * in a context - ie, it consists of a qconid applied to either a qvarid or
   * (qvarid applied to some types) *)
  and check_isclass ast =
    match ast.node with
    (* Make sure that class constraint is a 1-param application, and that lhs
     * of application is just a qconid *)
    | Ast0_btype_app ({
        node = Ast0_btype_atype {
          node = Ast0_atype_con {
            node = Ast0_gtycon_con c}}
        },a2) ->
          check_isclassrhs c a2
    | _ ->
        raise (Parse_error (Printf.sprintf2
          "Illegal context: at %d-%d\n"
          ast.blockstart ast.blockend))
  (* Take in an Ast0_type_*, and call check_isclass on the underlying btype
   * (error if it's more complicated than that) *)
  and check_typeisclass ast =
    match ast.node with
    | Ast0_type_btype a1 -> check_isclass a1
    | _ ->
        raise (Parse_error (Printf.sprintf2
          "Illegal context: at %d-%d\n"
          ast.blockstart ast.blockend))
  (* Take in an Ast0_leaf - to make the lhs of Ast1_class_* - and Ast0_atype_*
   * - to make the rhs - and check that the rhs is valid rhs of class
   * constraint. *)
  and check_isclassrhs c ast =
    let newnode = match ast.node with
    (* If rhs is just a var, easy *)
    | Ast0_atype_var a1 ->
        Ast1_class_var (postparse_check c, postparse_check a1)
    (* Otherwise, if parenthetical, we need to traverse the corresponding btype
     * (converting it to list of atypes) to check that the leftmost thing being
     * applied is a var *)
    | Ast0_atype_paren { node = Ast0_type_btype a1 } ->
        let atypes =
          let rec go bt acc =
            match bt with
            | Ast0_btype_atype a -> (a::acc)
            | Ast0_btype_app (b,a) -> go b.node (a::acc)
            | _ -> assert false
          in go a1.node []
        in
        match atypes with
        (* If so, build applied class constraint *)
        | { node = Ast0_atype_var a1 }::a2s ->
            Ast1_class_app (postparse_check c,
              postparse_check a1, (List.map postparse_check a2s))
        | _ ->
            raise (Parse_error (Printf.sprintf2
              "Illegal context: at %d-%d\n"
              ast.blockstart ast.blockend))
    | _ ->
        raise (Parse_error (Printf.sprintf2
          "Illegal context: at %d-%d\n"
          ast.blockstart ast.blockend))
    in do_bound ast newnode
  in
  match ast.node with
  (* Our grammar is written with arrows in types (-> or =>) having the lowest
   * precedence - and individual arrows just appear left to right. So we only
   * need to check the leftmost arrow. *)
  (*If it's a function (->), we know that
   * there cannot be a context (context cannot be a function type at top-level,
   * need an intervening class constructor). So just convert the ast. *)
  | Ast0_type_fun _
  (* Similarly if there are no arrows at all *)
  | Ast0_type_btype _ ->
      (None, postparse_check ast)
  (* If it's a context (=>), verify that the lhs is a context, and convert the
   * rhs ast. *)
  | Ast0_type_context (a1,a2) ->
      (Some (check_iscontext a1), postparse_check a2)
  | _ -> assert false

(* Take in an Ast0_exp_*, Ast0_infixexp_*, Ast0_exp10_*, or Ast0_aexp_*, and
 * verify that it is a valid pattern.  Return Ast1_pat_*, or (etc), modifying
 * all children accordingly. *)
and check_pat ast =
  let newnode = match ast.node with
  (* First, the valid cases. *)
  (* Convert exps -> pats *)
  (* Patterns cannot have type decls - (a1, Some _) is error. *)
  | Ast0_exp (a1, None) ->
      Ast1_pat (check_pat a1)
  (* infixexps -> infixpats *)
  | Ast0_infixexp_op (a1,a2,a3) ->
      (* Check that operator is qconop - qvarop is error *)
      match a2.node with
      | Ast0_backquoted_leaf { token = QConId }
      | Ast0_backquoted_leaf { token = ConId }
      | Ast0_leaf { token = QConSym }
      | Ast0_leaf { token = ConSym }
      | Ast0_leaf { token = RColon } ->
          Ast1_infixpat_op (check_pat a1, postparse_check a2, check_pat a3)
      | _ ->
          raise (Parse_error (Printf.sprintf2
            "Expected pattern, got expression: at %d-%d\n"
            ast.blockstart ast.blockend))
  (* exp10s -> pat10s *)
  | Ast0_infixexp_exp10 a1 ->
      Ast1_infixpat_pat10 (check_pat a1)
  (* A single pattern is fine ... *)
  | Ast0_exp10_aexps (a1::[]) ->
      Ast1_pat10_apat (check_pat a1)
  (* ... but can only apply one pattern to others if it's a constructor *)
  | Ast0_exp10_aexps ({ node = Ast0_aexp_con a1 }::a2s) ->
      Ast1_pat10_con (postparse_check a1, List.map check_pat a2s)
  (* Convert aexps to apats *)
  | Ast0_aexp_var a1 ->
      Ast1_apat_var (postparse_check a1)
  | Ast0_aexp_con a1 ->
      Ast1_apat_con (postparse_check a1)
  | Ast0_aexp_literal a1 ->
      Ast1_apat_literal (postparse_check a1)
  | Ast0_aexp_paren a1 ->
      Ast1_apat_paren (check_pat a1)
  | Ast0_aexp_tuple a1s ->
      Ast1_apat_tuple (List.map check_pat a1s)
  | Ast0_aexp_list a1s ->
      Ast1_apat_list (List.map check_pat a1s)
  | Ast0_aexp_lbupdate (a1,a2s) ->
      (* Check that head is qcon - anything more complex is error *)
      match a1.node with
      | Ast0_leaf { token = QConId }
      | Ast0_leaf { token = ConId }
      | Ast0_parenthesized_leaf { token = QConSym }
      | Ast0_parenthesized_leaf { token = ConSym }
      | Ast0_parenthesized_leaf { token = RColon } ->
          Ast1_apat_lbpat (postparse_check a1, List.map check_pat a2s)
      | _ ->
          raise (Parse_error (Printf.sprintf2
            "Expected pattern, got expression: at %d-%d\n"
            ast.blockstart ast.blockend))
  | Ast0_aexp_aspat (a1,a2) ->
      Ast1_apat_as (postparse_check a1, check_pat a2)
  | Ast0_aexp_irrefpat a1 ->
      Ast1_apat_irref (postparse_check a1)
  | Ast0_aexp_wildpat ->
      Ast1_apat_wild
  (* fbind -> fpat *)
  | Ast0_fbind (a1,a2) ->
      Ast1_fpat (postparse_check a1, check_pat a2)
  (* Finally, all other cases cannot appear in a pattern - error *)
  | _ ->
      raise (Parse_error (Printf.sprintf2
        "Expected pattern, got expression: at %d-%d\n"
        ast.blockstart ast.blockend))
  in do_bound ast newnode



(* Take in a (general) ast, and do context and pattern checks appropriately. *)
and postparse_check ast =
  (* Variants that actually require transformations at the top. All other
   * variants (we just convert directly from ast0 to ast1) listed after. *)
  let newnode = match ast.node with
  (* Check if bindings are funbind or patbind *)
  | Ast0_decl_bind (a1,a2) ->
      Ast1_decl_bind (check_bind a1,postparse_check a2)
  (* Check that all places where patterns appear are actually patterns *)
  | Ast0_exp10_lambda (a1s,a2) ->
      Ast1_exp10_lambda (List.map check_pat a1s,postparse_check a2)
  | Ast0_qual_assign (a1,a2) ->
      Ast1_qual_assign (check_pat a1,postparse_check a2)
  | Ast0_alt_match (a1,a2,oa3s) ->
      Ast1_alt_match (check_pat a1,postparse_check a2,Option.map (List.map postparse_check) oa3s)
  | Ast0_alt_guard (a1,a2s,oa3s) ->
      Ast1_alt_guard (check_pat a1,List.map postparse_check a2s,Option.map (List.map postparse_check) oa3s)
  | Ast0_stmt_assign (a1,a2) ->
      Ast1_stmt_assign (check_pat a1,postparse_check a2)
  (* Check that all places with optional contexts have either valid context, or
   * no context at all. *)
  | Ast0_decl_type (a1s,a2) ->
      match check_context a2 with
      | ctxt, t ->
          Ast1_decl_type (List.map postparse_check a1s, ctxt, t)
  | Ast0_exp (a1,oa2) ->
      Ast1_exp (postparse_check a1, Option.map check_context oa2)
  (* All of the following *cannot* appear in a general expression - can only
   * appear as a child of a pattern (handled above). So error. *)
  | Ast0_aexp_aspat _
  | Ast0_aexp_irrefpat _
  | Ast0_aexp_wildpat ->
      raise (Parse_error (Printf.sprintf2
        "Expected expression, got pattern: at %d-%d\n" ast.blockstart
        ast.blockend))
  (* Similarly, this *cannot* appear in a general type - only as child of
   * context-allowing type decl / typed exp. So error. *)
  | Ast0_type_context _ ->
      raise (Parse_error (Printf.sprintf2
        "Unexpected type context: at %d-%d\n" ast.blockstart
        ast.blockend))
  (* All of the following: directly convert ast0 to ast1 *)
  | Ast0_module (oa1, oa2s, a3) ->
      Ast1_module (Option.map postparse_check oa1, Option.map (List.map postparse_check) oa2s, postparse_check a3)
  | Ast0_body a1s ->
      Ast1_body (List.map postparse_check a1s)
  | Ast0_export_var a1 ->
      Ast1_export_var (postparse_check a1)
  | Ast0_export_type (a1,oa2s) ->
      Ast1_export_type (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_export_module a1 ->
      Ast1_export_module (postparse_check a1)
  | Ast0_topdecl_import (oa1,a2,Some (a3,a4),oa5) ->
      Ast1_topdecl_import (Option.map postparse_check oa1,postparse_check a2,
        Some (postparse_check a3,postparse_check a4),Option.map postparse_check oa5)
  | Ast0_topdecl_import (oa1,a2,None,oa5) ->
      Ast1_topdecl_import (Option.map postparse_check oa1,postparse_check a2,
        None,Option.map postparse_check oa5)
  | Ast0_impspec (oa1,a2s) ->
      Ast1_impspec (Option.map postparse_check oa1,List.map postparse_check a2s)
  | Ast0_import_var a1 ->
      Ast1_import_var (postparse_check a1)
  | Ast0_import_type (a1,oa2s) ->
      Ast1_import_type (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_topdecl_type (a1,a2) ->
      Ast1_topdecl_type (postparse_check a1,postparse_check a2)
  | Ast0_topdecl_data (a1,a2s,oa3) ->
      Ast1_topdecl_data (postparse_check a1,List.map postparse_check a2s,Option.map postparse_check oa3)
  | Ast0_topdecl_newtype (a1,a2,oa3) ->
      Ast1_topdecl_newtype (postparse_check a1,postparse_check a2,Option.map postparse_check oa3)
  | Ast0_topdecl_class (oa1,a2,a3,a4s) ->
      Ast1_topdecl_class (Option.map postparse_check oa1,postparse_check a2,postparse_check a3,List.map postparse_check a4s)
  | Ast0_topdecl_instance (oa1,a2,a3,a4s) ->
      Ast1_topdecl_instance (Option.map postparse_check oa1,postparse_check a2,postparse_check a3,List.map postparse_check a4s)
  | Ast0_topdecl_default a1s ->
      Ast1_topdecl_default (List.map postparse_check a1s)
  | Ast0_topdecl_decl a1 ->
      Ast1_topdecl_decl (postparse_check a1)
  | Ast0_decl_fixity (a1,oa2,a3s) ->
      Ast1_decl_fixity (postparse_check a1,Option.map postparse_check oa2,List.map postparse_check a3s)
  | Ast0_decl_empty ->
      Ast1_decl_empty
  | Ast0_type_fun (a1,a2) ->
      Ast1_type_fun (postparse_check a1,postparse_check a2)
  | Ast0_type_btype a1 ->
      Ast1_type_btype (postparse_check a1)
  | Ast0_btype_app (a1,a2) ->
      Ast1_btype_app (postparse_check a1,postparse_check a2)
  | Ast0_btype_atype a1 ->
      Ast1_btype_atype (postparse_check a1)
  | Ast0_atype_con a1 ->
      Ast1_atype_con (postparse_check a1)
  | Ast0_atype_var a1 ->
      Ast1_atype_var (postparse_check a1)
  | Ast0_atype_tuple a1s ->
      Ast1_atype_tuple (List.map postparse_check a1s)
  | Ast0_atype_list a1 ->
      Ast1_atype_list (postparse_check a1)
  | Ast0_atype_paren a1 ->
      Ast1_atype_paren (postparse_check a1)
  | Ast0_gtycon_con a1 ->
      Ast1_gtycon_con (postparse_check a1)
  | Ast0_gtycon_unit ->
      Ast1_gtycon_unit
  | Ast0_gtycon_list ->
      Ast1_gtycon_unit
  | Ast0_gtycon_fun ->
      Ast1_gtycon_fun
  | Ast0_gtycon_tuple a1s ->
      Ast1_gtycon_tuple (List.map postparse_check a1s)
  | Ast0_scontext a1s ->
      Ast1_scontext (List.map postparse_check a1s)
  | Ast0_simpleclass (a1,a2) ->
      Ast1_simpleclass (postparse_check a1,postparse_check a2)
  | Ast0_simpletype (a1,a2s) ->
      Ast1_simpletype (postparse_check a1,List.map postparse_check a2s)
  | Ast0_constr_con a1 ->
      Ast1_constr_con (postparse_check a1)
  | Ast0_constr_conop (a1,a2,a3) ->
      Ast1_constr_conop (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_constr_fields (a1,a2s) ->
      Ast1_constr_fields (postparse_check a1,List.map postparse_check a2s)
  | Ast0_newconstr_con (a1,a2) ->
      Ast1_newconstr_con (postparse_check a1,postparse_check a2)
  | Ast0_newconstr_field (a1,a2,a3) ->
      Ast1_newconstr_field (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_fielddecl (a1s,a2) ->
      Ast1_fielddecl (List.map postparse_check a1s,postparse_check a2)
  | Ast0_deriving a1s ->
      Ast1_deriving (List.map postparse_check a1s)
  | Ast0_inst_con a1 ->
      Ast1_inst_con (postparse_check a1)
  | Ast0_inst_app (a1,a2s) ->
      Ast1_inst_app (postparse_check a1,List.map postparse_check a2s)
  | Ast0_inst_tuple a1s ->
      Ast1_inst_tuple (List.map postparse_check a1s)
  | Ast0_inst_list a1 ->
      Ast1_inst_list (postparse_check a1)
  | Ast0_inst_fun (a1,a2) ->
      Ast1_inst_fun (postparse_check a1,postparse_check a2)
  | Ast0_rhs_eq (a1,oa2s) ->
      Ast1_rhs_eq (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_rhs_guard (a1s,oa2s) ->
      Ast1_rhs_guard (List.map postparse_check a1s,Option.map (List.map postparse_check) oa2s)
  | Ast0_gdrhs (a1,a2) ->
      Ast1_gdrhs (postparse_check a1,postparse_check a2)
  | Ast0_infixexp_op (a1,a2,a3) ->
      Ast1_infixexp_op (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_infixexp_exp10 a1 ->
      Ast1_infixexp_exp10 (postparse_check a1)
  | Ast0_exp10_let (a1s,a2) ->
      Ast1_exp10_let (List.map postparse_check a1s,postparse_check a2)
  | Ast0_exp10_if (a1,a2,a3) ->
      Ast1_exp10_if (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_exp10_case (a1,a2s) ->
      Ast1_exp10_case (postparse_check a1,List.map postparse_check a2s)
  | Ast0_exp10_do a1s ->
      Ast1_exp10_do (List.map postparse_check a1s)
  | Ast0_exp10_aexps a1s ->
      Ast1_exp10_aexps (List.map postparse_check a1s)
  | Ast0_aexp_var a1 ->
      Ast1_aexp_var (postparse_check a1)
  | Ast0_aexp_con a1 ->
      Ast1_aexp_con (postparse_check a1)
  | Ast0_aexp_literal a1 ->
      Ast1_aexp_literal (postparse_check a1)
  | Ast0_aexp_paren a1 ->
      Ast1_aexp_paren (postparse_check a1)
  | Ast0_aexp_tuple a1s ->
      Ast1_aexp_tuple (List.map postparse_check a1s)
  | Ast0_aexp_list a1s ->
      Ast1_aexp_list (List.map postparse_check a1s)
  | Ast0_aexp_seq (a1, oa2, oa3) ->
      Ast1_aexp_seq (postparse_check a1, Option.map postparse_check oa2, Option.map postparse_check oa3)
  | Ast0_aexp_comp (a1,a2s) ->
      Ast1_aexp_comp (postparse_check a1,List.map postparse_check a2s)
  | Ast0_aexp_lsec (a1,a2) ->
      Ast1_aexp_lsec (postparse_check a1,postparse_check a2)
  | Ast0_aexp_rsec (a1,a2) ->
      Ast1_aexp_rsec (postparse_check a1,postparse_check a2)
  | Ast0_aexp_lbupdate (a1,a2s) ->
      Ast1_aexp_lbupdate (postparse_check a1,List.map postparse_check a2s)
  | Ast0_qual_let a1s ->
      Ast1_qual_let (List.map postparse_check a1s)
  | Ast0_qual_guard a1 ->
      Ast1_qual_guard (postparse_check a1)
  | Ast0_gdpat (a1,a2) ->
      Ast1_gdpat (postparse_check a1,postparse_check a2)
  | Ast0_stmt_exp a1 ->
      Ast1_stmt_exp (postparse_check a1)
  | Ast0_stmt_let a1s ->
      Ast1_stmt_let (List.map postparse_check a1s)
  | Ast0_stmt_empty ->
      (Ast0_stmt_empty)
  | Ast0_fbind (a1, a2) ->
      Ast1_fbind (postparse_check a1, postparse_check a2)
  | Ast0_gcon_unit ->
      Ast1_gcon_unit
  | Ast0_gcon_list ->
      Ast1_gcon_list
  | Ast0_gcon_tuple a1s ->
      Ast1_gcon_tuple (List.map postparse_check a1s)
  | Ast0_gcon_qcon a1 ->
      Ast1_gcon_qcon (postparse_check a1)
  | Ast0_parenthesized_leaf l ->
      Ast1_parenthesized_leaf l
  | Ast0_backquoted_leaf l ->
      Ast1_backquoted_leaf l
  | Ast0_leaf l ->
      Ast1_leaf l
  | Ast0_partial_list _ -> assert false
  in do_bound ast newnode
;;
