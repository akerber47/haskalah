open Batteries
;;
open Types
;;

let ast0_do_bounds child_asts parent_astnode =
  let min_start = List.min (List.map (fun ast -> ast.blockstart) child_asts)
  and max_end   = List.max (List.map (fun ast -> ast.blockend) child_asts)
  in { node = parent_astnode; blockstart = min_start; blockend = max_end; }
;;

let rec ast0_general_foldl_helper pref postf depth acc ast =
  let acc1 = pref acc ast depth in
  (* Dig through each node struct, and pull out list of children *)
  let children =
    match ast.node with
    | Ast0_module (Some a1, Some a2s, a3) -> (a1::a2s) @ [a3]
    | Ast0_module (Some a1, None, a3) -> [a1;a3]
    | Ast0_module (None, Some a2s, a3) -> a2s @ [a3]
    | Ast0_module (None, None, a3) -> [a3]
    | Ast0_body a1s -> a1s
    | Ast0_export_var a1 -> [a1]
    | Ast0_export_type (a1,Some a2s) -> a1::a2s
    | Ast0_export_type (a1,None) -> [a1]
    | Ast0_export_module a1 -> [a1]
    | Ast0_topdecl_import (Some a1,a2,Some (a3,a4),Some a5) -> [a1;a2;a3;a4;a5]
    | Ast0_topdecl_import (Some a1,a2,Some (a3,a4),None) -> [a1;a2;a3;a4]
    | Ast0_topdecl_import (Some a1,a2,None,Some a5) -> [a1;a2;a5]
    | Ast0_topdecl_import (Some a1,a2,None,None) -> [a1;a2]
    | Ast0_topdecl_import (None,a2,Some (a3,a4),Some a5) -> [a2;a3;a4;a5]
    | Ast0_topdecl_import (None,a2,Some (a3,a4),None) -> [a2;a3;a4]
    | Ast0_topdecl_import (None,a2,None,Some a5) -> [a2;a5]
    | Ast0_topdecl_import (None,a2,None,None) -> [a2]
    | Ast0_impspec (Some a1,a2s) -> a1::a2s
    | Ast0_impspec (None,a2s) -> a2s
    | Ast0_import_var a1 -> [a1]
    | Ast0_import_type (a1,Some a2s) -> a1::a2s
    | Ast0_import_type (a1,None) -> [a1]
    | Ast0_topdecl_type (a1,a2) -> [a1;a2]
    | Ast0_topdecl_data (a1,a2s,Some a3) -> (a1::a2s) @ [a3]
    | Ast0_topdecl_data (a1,a2s,None) -> (a1::a2s)
    | Ast0_topdecl_newtype (a1,a2,Some a3) -> [a1;a2;a3]
    | Ast0_topdecl_newtype (a1,a2,None) -> [a1;a2]
    | Ast0_topdecl_class (Some a1,a2,a3,a4s) -> a1::(a2::(a3::a4s))
    | Ast0_topdecl_class (None,a2,a3,a4s) -> a2::(a3::a4s)
    | Ast0_topdecl_instance (Some a1,a2,a3,a4s) -> a1::(a2::(a3::a4s))
    | Ast0_topdecl_instance (None,a2,a3,a4s) -> a2::(a3::a4s)
    | Ast0_topdecl_default a1s -> a1s
    | Ast0_topdecl_decl a1 -> [a1]
    | Ast0_decl_bind (a1,a2) -> [a1;a2]
    | Ast0_decl_type (a1s,a2) -> a1s @ [a2]
    | Ast0_decl_fixity (a1,Some a2,a3s) -> a1::(a2::a3s)
    | Ast0_decl_fixity (a1,None,a3s) -> a1::a3s
    | Ast0_decl_empty -> []
    | Ast0_type_context (a1,a2) -> [a1;a2]
    | Ast0_type_fun (a1,a2) -> [a1;a2]
    | Ast0_type_btype a1 -> [a1]
    | Ast0_btype_app (a1,a2) -> [a1;a2]
    | Ast0_btype_atype a1 -> [a1]
    | Ast0_atype_con a1 -> [a1]
    | Ast0_atype_var a1 -> [a1]
    | Ast0_atype_tuple a1s -> a1s
    | Ast0_atype_list a1 -> [a1]
    | Ast0_atype_paren a1 -> [a1]
    | Ast0_gtycon_con a1 -> [a1]
    | Ast0_gtycon_unit
    | Ast0_gtycon_list
    | Ast0_gtycon_fun -> []
    | Ast0_gtycon_tuple a1s -> a1s
    | Ast0_scontext a1s -> a1s
    | Ast0_simpleclass (a1,a2) -> [a1;a2]
    | Ast0_simpletype (a1,a2s) -> a1::a2s
    | Ast0_constr_con a1 -> [a1]
    | Ast0_constr_conop (a1,a2,a3) -> [a1;a2;a3]
    | Ast0_constr_fields (a1,a2s) -> a1::a2s
    | Ast0_newconstr_con (a1,a2) -> [a1;a2]
    | Ast0_newconstr_field (a1,a2,a3) -> [a1;a2;a3]
    | Ast0_fielddecl (a1s,a2) -> a1s @ [a2]
    | Ast0_deriving a1s -> a1s
    | Ast0_inst_con a1 -> [a1]
    | Ast0_inst_app (a1,a2s) -> a1::a2s
    | Ast0_inst_tuple a1s -> a1s
    | Ast0_inst_list a1 -> [a1]
    | Ast0_inst_fun (a1,a2) -> [a1;a2]
    | Ast0_rhs_eq (a1,Some a2s) -> a1::a2s
    | Ast0_rhs_eq (a1,None) -> [a1]
    | Ast0_rhs_guard (a1s,Some a2s) -> a1s @ a2s
    | Ast0_rhs_guard (a1s,None) -> a1s
    | Ast0_gdrhs (a1,a2) -> [a1;a2]
    | Ast0_exp (a1,Some a2) -> [a1;a2]
    | Ast0_exp (a1,None) -> [a1]
    | Ast0_infixexp_op (a1,a2,a3) -> [a1;a2;a3]
    | Ast0_infixexp_exp10 a1 -> [a1]
    | Ast0_exp10_lambda (a1s,a2) -> a1s @ [a2]
    | Ast0_exp10_let (a1s,a2) -> a1s @ [a2]
    | Ast0_exp10_if (a1,a2,a3) -> [a1;a2;a3]
    | Ast0_exp10_case (a1,a2s) -> a1::a2s
    | Ast0_exp10_do a1s -> a1s
    | Ast0_exp10_aexps a1s -> a1s
    | Ast0_aexp_var a1 -> [a1]
    | Ast0_aexp_con a1 -> [a1]
    | Ast0_aexp_literal a1 -> [a1]
    | Ast0_aexp_paren a1 -> [a1]
    | Ast0_aexp_tuple a1s -> a1s
    | Ast0_aexp_list a1s -> a1s
    | Ast0_aexp_seq (a1, Some a2, Some a3) -> [a1;a2;a3]
    | Ast0_aexp_seq (a1, Some a2, None) -> [a1;a2]
    | Ast0_aexp_seq (a1, None, Some a3) -> [a1;a3]
    | Ast0_aexp_seq (a1, None, None) -> [a1]
    | Ast0_aexp_comp (a1,a2s) -> a1::a2s
    | Ast0_aexp_lsec (a1,a2) -> [a1;a2]
    | Ast0_aexp_rsec (a1,a2) -> [a1;a2]
    | Ast0_aexp_lbupdate (a1,a2s) -> a1::a2s
    | Ast0_aexp_aspat (a1,a2) -> [a1;a2]
    | Ast0_aexp_irrefpat a1 -> [a1]
    | Ast0_aexp_wildpat -> []
    | Ast0_qual_assign (a1,a2) -> [a1;a2]
    | Ast0_qual_let a1s -> a1s
    | Ast0_qual_guard a1 -> [a1]
    | Ast0_alt_match (a1,a2,Some a3s) -> a1::(a2::a3s)
    | Ast0_alt_match (a1,a2,None) -> [a1;a2]
    | Ast0_alt_guard (a1,a2s,Some a3s) -> (a1::a2s) @ a3s
    | Ast0_alt_guard (a1,a2s,None) -> (a1::a2s)
    | Ast0_gdpat (a1,a2) -> [a1;a2]
    | Ast0_stmt_exp a1 -> [a1]
    | Ast0_stmt_assign (a1,a2) -> [a1;a2]
    | Ast0_stmt_let a1s -> a1s
    | Ast0_stmt_empty -> []
    | Ast0_fbind (a1, a2) -> [a1;a2]
    | Ast0_gcon_unit
    | Ast0_gcon_list -> []
    | Ast0_gcon_tuple a1s -> a1s
    | Ast0_gcon_qcon a1 -> [a1]
    | Ast0_parenthesized_leaf _
    | Ast0_backquoted_leaf _
    | Ast0_leaf _ -> []
    | Ast0_partial_list a1s -> a1s
  in
  let acc2 = List.fold_left (ast0_general_foldl_helper pref postf (depth + 1))
                            acc1
                            children
  in
  postf acc2 ast depth
;;

let ast0_general_foldl pref postf acc ast =
  ast0_general_foldl_helper pref postf 0 acc ast
;;

let foldl_id acc _ _ = acc
;;

let ast0_preorder_foldl f acc ast =
  ast0_general_foldl f foldl_id acc ast
;;

let ast0_postorder_foldl f acc ast =
  ast0_general_foldl foldl_id f acc ast
;;
