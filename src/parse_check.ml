open Batteries
;;
open Types
;;

let rec postparse_check ast =
  let newnode = match ast.node with
  | Ast0_module (oa1, oa2s, a3) ->
      Ast0_module (Option.map postparse_check oa1, Option.map (List.map postparse_check) oa2s, postparse_check a3)
  | Ast0_body a1s ->
      Ast0_body (List.map postparse_check a1s)
  | Ast0_export_var a1 ->
      Ast0_export_var (postparse_check a1)
  | Ast0_export_type (a1,oa2s) ->
      Ast0_export_type (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_export_module a1 ->
      Ast0_export_module (postparse_check a1)
  | Ast0_topdecl_import (oa1,a2,Some (a3,a4),oa5) ->
      Ast0_topdecl_import (Option.map postparse_check oa1,postparse_check a2,
        Some (postparse_check a3,postparse_check a4),Option.map postparse_check oa5)
  | Ast0_topdecl_import (oa1,a2,None,oa5) ->
      Ast0_topdecl_import (Option.map postparse_check oa1,postparse_check a2,
        None,Option.map postparse_check oa5)
  | Ast0_impspec (oa1,a2s) ->
      Ast0_impspec (Option.map postparse_check oa1,List.map postparse_check a2s)
  | Ast0_import_var a1 ->
      Ast0_import_var (postparse_check a1)
  | Ast0_import_type (a1,oa2s) ->
      Ast0_import_type (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_topdecl_type (a1,a2) ->
      Ast0_topdecl_type (postparse_check a1,postparse_check a2)
  | Ast0_topdecl_data (a1,a2s,oa3) ->
      Ast0_topdecl_data (postparse_check a1,List.map postparse_check a2s,Option.map postparse_check oa3)
  | Ast0_topdecl_newtype (a1,a2,oa3) ->
      Ast0_topdecl_newtype (postparse_check a1,postparse_check a2,Option.map postparse_check oa3)
  | Ast0_topdecl_class (oa1,a2,a3,a4s) ->
      Ast0_topdecl_class (Option.map postparse_check oa1,postparse_check a2,postparse_check a3,List.map postparse_check a4s)
  | Ast0_topdecl_instance (oa1,a2,a3,a4s) ->
      Ast0_topdecl_instance (Option.map postparse_check oa1,postparse_check a2,postparse_check a3,List.map postparse_check a4s)
  | Ast0_topdecl_default a1s ->
      Ast0_topdecl_default (List.map postparse_check a1s)
  | Ast0_topdecl_decl a1 ->
      Ast0_topdecl_decl (postparse_check a1)
  | Ast0_decl_bind (a1,a2) ->
      Ast0_decl_bind (postparse_check a1,postparse_check a2)
  | Ast0_decl_type (a1s,a2) ->
      Ast0_decl_type (List.map postparse_check a1s,postparse_check a2)
  | Ast0_decl_fixity (a1,oa2,a3s) ->
      Ast0_decl_fixity (postparse_check a1,Option.map postparse_check oa2,List.map postparse_check a3s)
  | Ast0_decl_empty ->
      Ast0_decl_empty
  | Ast0_type_context (a1,a2) ->
      Ast0_type_context (postparse_check a1,postparse_check a2)
  | Ast0_type_fun (a1,a2) ->
      Ast0_type_fun (postparse_check a1,postparse_check a2)
  | Ast0_type_btype a1 ->
      Ast0_type_btype (postparse_check a1)
  | Ast0_btype_app (a1,a2) ->
      Ast0_btype_app (postparse_check a1,postparse_check a2)
  | Ast0_btype_atype a1 ->
      Ast0_btype_atype (postparse_check a1)
  | Ast0_atype_con a1 ->
      Ast0_atype_con (postparse_check a1)
  | Ast0_atype_var a1 ->
      Ast0_atype_var (postparse_check a1)
  | Ast0_atype_tuple a1s ->
      Ast0_atype_tuple (List.map postparse_check a1s)
  | Ast0_atype_list a1 ->
      Ast0_atype_list (postparse_check a1)
  | Ast0_atype_paren a1 ->
      Ast0_atype_paren (postparse_check a1)
  | Ast0_gtycon_con a1 ->
      Ast0_gtycon_con (postparse_check a1)
  | Ast0_gtycon_unit ->
      Ast0_gtycon_unit
  | Ast0_gtycon_list ->
      Ast0_gtycon_unit
  | Ast0_gtycon_fun ->
      Ast0_gtycon_fun
  | Ast0_gtycon_tuple a1s ->
      Ast0_gtycon_tuple (List.map postparse_check a1s)
  | Ast0_scontext a1s ->
      Ast0_scontext (List.map postparse_check a1s)
  | Ast0_simpleclass (a1,a2) ->
      Ast0_simpleclass (postparse_check a1,postparse_check a2)
  | Ast0_simpletype (a1,a2s) ->
      Ast0_simpletype (postparse_check a1,List.map postparse_check a2s)
  | Ast0_constr_con a1 ->
      Ast0_constr_con (postparse_check a1)
  | Ast0_constr_conop (a1,a2,a3) ->
      Ast0_constr_conop (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_constr_fields (a1,a2s) ->
      Ast0_constr_fields (postparse_check a1,List.map postparse_check a2s)
  | Ast0_newconstr_con (a1,a2) ->
      Ast0_newconstr_con (postparse_check a1,postparse_check a2)
  | Ast0_newconstr_field (a1,a2,a3) ->
      Ast0_newconstr_field (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_fielddecl (a1s,a2) ->
      Ast0_fielddecl (List.map postparse_check a1s,postparse_check a2)
  | Ast0_deriving a1s ->
      Ast0_deriving (List.map postparse_check a1s)
  | Ast0_inst_con a1 ->
      Ast0_inst_con (postparse_check a1)
  | Ast0_inst_app (a1,a2s) ->
      Ast0_inst_app (postparse_check a1,List.map postparse_check a2s)
  | Ast0_inst_tuple a1s ->
      Ast0_inst_tuple (List.map postparse_check a1s)
  | Ast0_inst_list a1 ->
      Ast0_inst_list (postparse_check a1)
  | Ast0_inst_fun (a1,a2) ->
      Ast0_inst_fun (postparse_check a1,postparse_check a2)
  | Ast0_rhs_eq (a1,oa2s) ->
      Ast0_rhs_eq (postparse_check a1,Option.map (List.map postparse_check) oa2s)
  | Ast0_rhs_guard (a1s,oa2s) ->
      Ast0_rhs_guard (List.map postparse_check a1s,Option.map (List.map postparse_check) oa2s)
  | Ast0_gdrhs (a1,a2) ->
      Ast0_gdrhs (postparse_check a1,postparse_check a2)
  | Ast0_exp (a1,oa2) ->
      Ast0_exp (postparse_check a1,Option.map postparse_check oa2)
  | Ast0_infixexp_op (a1,a2,a3) ->
      Ast0_infixexp_op (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_infixexp_exp10 a1 ->
      Ast0_infixexp_exp10 (postparse_check a1)
  | Ast0_exp10_lambda (a1s,a2) ->
      Ast0_exp10_lambda (List.map postparse_check a1s,postparse_check a2)
  | Ast0_exp10_let (a1s,a2) ->
      Ast0_exp10_let (List.map postparse_check a1s,postparse_check a2)
  | Ast0_exp10_if (a1,a2,a3) ->
      Ast0_exp10_if (postparse_check a1,postparse_check a2,postparse_check a3)
  | Ast0_exp10_case (a1,a2s) ->
      Ast0_exp10_case (postparse_check a1,List.map postparse_check a2s)
  | Ast0_exp10_do a1s ->
      Ast0_exp10_do (List.map postparse_check a1s)
  | Ast0_exp10_aexps a1s ->
      Ast0_exp10_aexps (List.map postparse_check a1s)
  | Ast0_aexp_var a1 ->
      Ast0_aexp_var (postparse_check a1)
  | Ast0_aexp_con a1 ->
      Ast0_aexp_con (postparse_check a1)
  | Ast0_aexp_literal a1 ->
      Ast0_aexp_literal (postparse_check a1)
  | Ast0_aexp_paren a1 ->
      Ast0_aexp_paren (postparse_check a1)
  | Ast0_aexp_tuple a1s ->
      Ast0_aexp_tuple (List.map postparse_check a1s)
  | Ast0_aexp_list a1s ->
      Ast0_aexp_list (List.map postparse_check a1s)
  | Ast0_aexp_seq (a1, oa2, oa3) ->
      Ast0_aexp_seq (postparse_check a1, Option.map postparse_check oa2, Option.map postparse_check oa3)
  | Ast0_aexp_comp (a1,a2s) ->
      Ast0_aexp_comp (postparse_check a1,List.map postparse_check a2s)
  | Ast0_aexp_lsec (a1,a2) ->
      Ast0_aexp_lsec (postparse_check a1,postparse_check a2)
  | Ast0_aexp_rsec (a1,a2) ->
      Ast0_aexp_rsec (postparse_check a1,postparse_check a2)
  | Ast0_aexp_lbupdate (a1,a2s) ->
      Ast0_aexp_lbupdate (postparse_check a1,List.map postparse_check a2s)
  | Ast0_aexp_aspat (a1,a2) ->
      Ast0_aexp_aspat (postparse_check a1,postparse_check a2)
  | Ast0_aexp_irrefpat a1 ->
      Ast0_aexp_irrefpat (postparse_check a1)
  | Ast0_aexp_wildpat ->
      Ast0_aexp_wildpat
  | Ast0_qual_assign (a1,a2) ->
      Ast0_qual_assign (postparse_check a1,postparse_check a2)
  | Ast0_qual_let a1s ->
      Ast0_qual_let (List.map postparse_check a1s)
  | Ast0_qual_guard a1 ->
      Ast0_qual_guard (postparse_check a1)
  | Ast0_alt_match (a1,a2,oa3s) ->
      Ast0_alt_match (postparse_check a1,postparse_check a2,Option.map (List.map postparse_check) oa3s)
  | Ast0_alt_guard (a1,a2s,oa3s) ->
      Ast0_alt_guard (postparse_check a1,List.map postparse_check a2s,Option.map (List.map postparse_check) oa3s)
  | Ast0_gdpat (a1,a2) ->
      Ast0_gdpat (postparse_check a1,postparse_check a2)
  | Ast0_stmt_exp a1 ->
      Ast0_stmt_exp (postparse_check a1)
  | Ast0_stmt_assign (a1,a2) ->
      Ast0_stmt_assign (postparse_check a1,postparse_check a2)
  | Ast0_stmt_let a1s ->
      Ast0_stmt_let (List.map postparse_check a1s)
  | Ast0_stmt_empty ->
      (Ast0_stmt_empty)
  | Ast0_fbind (a1, a2) ->
      Ast0_fbind (postparse_check a1, postparse_check a2)
  | Ast0_gcon_unit ->
      Ast0_gcon_unit
  | Ast0_gcon_list ->
      Ast0_gcon_list
  | Ast0_gcon_tuple a1s ->
      Ast0_gcon_tuple (List.map postparse_check a1s)
  | Ast0_gcon_qcon a1 ->
      Ast0_gcon_qcon (postparse_check a1)
  | Ast0_parenthesized_leaf l ->
      Ast0_parenthesized_leaf l
  | Ast0_backquoted_leaf l ->
      Ast0_backquoted_leaf l
  | Ast0_leaf l ->
      Ast0_leaf l
  | Ast0_partial_list _ -> assert false
  in { node = newnode; blockstart = ast.blockstart; blockend = ast.blockend }
;;
