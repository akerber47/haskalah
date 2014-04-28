open Batteries
;;
open Types
;;

type associativity =
  | Left
  | Right
  | Non
;;
type fixity = associativity * int

(* To avoid having to do a second (!) descent just to handle fixities, we store
 * all operator fixities (by name) in a global lookup table. Yeah, this is
 * really hacky / not functionally pure, but whatever. *)
let HASHTABLE_SIZE = 500
;;
(* (name, associativity * int) Hashtbl.t *)
let fixities = Hashtbl.create HASHTABLE_SIZE
;;

(* Lookup the fixity of an operator (by name). If not found, return the default
 * (left associative, precedence 9)
 * ast <Ast1_rleaf <Rleaf_name>> -> fixity *)
let get_fixity ast =
  match ast.node with
  | Ast1_rleaf (Rleaf_name nm) ->
    if Hashtbl.mem fixities nm then
      Hashtbl.find fixities nm
    else
      (Left, 9)
  | _ -> assert false
;;

let declare ast =
  let (asc, prec, ops) =
    match ast.node with
    (* Pull out the precedence integer and parse it. Note that this is the ONLY
     * time we actually parse and convert (not just validate) literals in the
     * entire compiler, as we need an integer value at compile time. *)
    | Ast1_decl_fixity (asc, Some { node1 = Ast1_rleaf (
        Rleaf_literal (Lit_int, i)); _}, ops) ->
          let p =
            try int_of_string i with
            | _ ->
                raise (Fixity_error (Printf.sprintf2
                  "Invalid operator precedence '%s' at (%d,%d)"
                  i ast.blockstart1 ast.blockend1))
          in begin
            if p < 0 || p > 9 then
              raise (Fixity_error (Printf.sprintf2
                "Invalid operator precedence '%s' at (%d,%d)"
                i ast.blockstart1 ast.blockend1));
            (asc, p, ops)
          end
    (* Otherwise, assume default precedence of 9 *)
    | Ast1_decl_fixity (asc, None, ops) ->
        (asc, 9, ops)
    | _ -> assert false
  in
  let assoc =
    match asc.node with
    | Ast1_leaf { token = RInfixl; _} -> Left
    | Ast1_leaf { token = RInfixr; _} -> Right
    | Ast1_leaf { token = RInfix; _ } -> Non
    | _ -> assert false
  in
  (* Add all the given operators with the given fixity. If any operators
   * already have an assigned fixity, error. *)
  List.iter
    (fun (Ast1_rleaf (Rleaf_name opname)) -> begin
      if Hashtbl.mem fixities opname then
        raise (Fixity_error (Printf.sprintf2
          "Cannot redefine fixity of operator: at (%d,%d)"
          ast.blockstart1 ast.blockend1));
      Hashtbl.add fixities opname (assoc, prec)
    end)
    ops
;;

let rec resolve ast =
  (* Descend recursively, as usual. We only care about transforming
   * infixexps and infixpats. *)
  let newnode =
    match ast.node with
    (* ignore exports *)
    | Ast1_module (oa1, oa2s, a3) ->
        Ast1_module (oa1, oa2s, resolve a3)
    | Ast1_body a1s ->
        Ast1_body (List.map resolve a1s)
    (* ignore all top declarations except class / instance bodies
     * and general declarations. *)
    | Ast1_topdecl_import (oa1, a2, oa34, oa5) ->
        Ast1_topdecl_import (oa1, a2, oa34, oa5)
    | Ast1_topdecl_type (a1,a2) ->
        Ast1_topdecl_type (a1,a2)
    | Ast1_topdecl_data (a1,a2s,oa3) ->
        Ast1_topdecl_data (a1,a2s,oa3)
    | Ast1_topdecl_newtype (a1,a2,oa3) ->
        Ast1_topdecl_newtype (a1,a2,oa3)
    | Ast1_topdecl_class (oa1,a2,a3,a4s) ->
        Ast1_topdecl_class (oa1,a2,a3, List.map resolve a4s)
    | Ast1_topdecl_instance (oa1,a2,a3,a4s) ->
        Ast1_topdecl_instance (oa1,a2,a3, List.map resolve a4s)
    | Ast1_topdecl_default a1s ->
        Ast1_topdecl_default a1s
    | Ast1_topdecl_decl a1 ->
        Ast1_topdecl_decl (resolve a1)
    (* ignore all declarations except bindings *)
    | Ast1_decl_funbind (a1,a2) ->
        Ast1_decl_funbind (resolve a1, resolve a2)
    | Ast1_decl_patbind (a1,a2) ->
        Ast1_decl_patbind (resolve a1, resolve a2)
    | Ast1_decl_type (a1s,a2) ->
        Ast1_decl_type (a1s,a2)
    | Ast1_decl_fixity (a1,oa2,a3s) ->
        Ast1_decl_fixity (a1,oa2,a3s)
    | Ast1_decl_empty ->
        Ast1_decl_empty
    | Ast1_funlhs_fun (a1,a2s) ->
        Ast1_funlhs_fun (a1, List.map resolve a2s)
    (* When declaring a function with operator syntax, check that argument
     * patterns do not have lower-precedence operators than declared function
     * (and if same precedence, correct associativity) *)
    | Ast1_funlhs_funop (a1,a2,a3) ->
        let (a,i) = get_fixity a2 in
        match a with
        | Left ->
            Ast1_funlhs_funop (do_infixpat (Left,i) a1, a2,
              do_infixpat (Non,i) a3)
        | Right ->
            Ast1_funlhs_funop (do_infixpat (Non,i) a1, a2,
              do_infixpat (Right,i) a3)
        | Non ->
            Ast1_funlhs_funop (do_infixpat (Non,i) a1, a2,
              do_infixpat (Non,i) a3)
    | Ast1_funlhs_nested (a1,a2s) ->
        Ast1_funlhs_nested (resolve a1, List.map resolve a2s)
    | Ast1_rhs_eq (a1,oa2s) ->
        Ast1_rhs_eq (resolve a1, Option.map (List.map resolve) oa2s)
    | Ast1_rhs_guard (a1s,oa2s) ->
        Ast1_rhs_guard (List.map resolve a1s,
          Option.map (List.map resolve) oa2s)
    | Ast1_gdrhs (a1,a2) ->
        Ast1_gdrhs (resolve a1, resolve a2)
    | Ast1_exp (a1,oa2) ->
        Ast1_exp (resolve a1, oa2)
    (* The key cases *)
    | Ast1_infixexp_op _
    | Ast1_infixexp_exp10 _ ->
        (do_infixexp (Non,~-1) ast).node1
    (* For all deeper expressions, look for pieces that might contain operators
     * inside parenthesized higher-up expressions *)
    | Ast1_exp10_lambda (a1s,a2) ->
        Ast1_exp10_lambda (List.map resolve a1s, resolve a2)
    | Ast1_exp10_let (a1s,a2) ->
        Ast1_exp10_let (List.map resolve a1s, resolve a2)
    | Ast1_exp10_if (a1,a2,a3) ->
        Ast1_exp10_if (resolve a1, resolve a2, resolve a3)
    | Ast1_exp10_case (a1,a2s) ->
        Ast1_exp10_case (resolve a1, resolve a2, resolve a3)
    | Ast1_exp10_do a1s ->
        Ast1_exp10_do (List.map resolve a1s)
    | Ast1_exp10_aexps a1s ->
        Ast1_exp10_aexps (List.map resolve a1s)
    | Ast1_aexp_var a1 ->
        Ast1_aexp_var a1
    | Ast1_aexp_con a1 ->
        Ast1_aexp_var a1
    | Ast1_aexp_literal a1 ->
        Ast1_aexp_literal a1
    | Ast1_aexp_paren a1 ->
        Ast1_aexp_paren (resolve a1)
    | Ast1_aexp_tuple a1s ->
        Ast1_aexp_tuple (List.map resolve a1s)
    | Ast1_aexp_list a1s ->
        Ast1_aexp_list (List.map resolve a1s)
    | Ast1_aexp_seq (a1, oa2, oa3) ->
        Ast1_aexp_seq (resolve a1, Option.map resolve a2,
          Option.map resolve a3)
    | Ast1_aexp_comp (a1,a2s) ->
        Ast1_aexp_comp (resolve a1, List.map resolve a2)
    (* For sections, check that argument expression does not have
     * lower-precedence operators than operator we are taking section of
     * (and if same precedence, correct associativity) *)
    | Ast1_aexp_lsec (a1,a2) ->
        let (a,i) = get_fixity a2 in
        if a == Left then
          Ast1_aexp_lsec (do_infixexp (Left,i) a1, a2)
        else
          Ast1_aexp_lsec (do_infixexp (Non,i) a1, a2)
    | Ast1_aexp_rsec (a1,a2) ->
        let (a,i) = get_fixity a1 in
        if a == Right then
          Ast1_aexp_rsec (a1, do_infixexp (Right,i) a2)
        else
          Ast1_aexp_rsec (a1, do_infixexp (Non,i) a1)
    | Ast1_aexp_lbupdate (a1,a2s) ->
        Ast1_aexp_lbupdate (resolve a1, List.map resolve a2s)
    | Ast1_qual_assign (a1,a2) ->
        Ast1_qual_assign (resolve a1, resolve a2)
    | Ast1_qual_let a1s ->
        Ast1_qual_let (List.map resolve a1s)
    | Ast1_qual_guard a1 ->
        Ast1_qual_guard (resolve a1)
    | Ast1_alt_match (a1,a2,oa3s) ->
        Ast1_alt_match (resolve a1, resolve a2,
          Option.map (List.map resolve) oa3s)
    | Ast1_alt_guard (a1,a2s,oa3s) ->
        Ast1_alt_guard (resolve a1, List.map resolve a2s,
          Option.map (List.map resolve) oa3s)
    | Ast1_gdpat (a1,a2) ->
        Ast1_gdpat (resolve a1, resolve a2)
    | Ast1_stmt_exp a1 ->
        Ast1_stmt_exp (resolve a1)
    | Ast1_stmt_assign (a1,a2) ->
        Ast1_stmt_assign (resolve a1, resolve a2)
    | Ast1_stmt_let a1s ->
        Ast1_stmt_let (List.map resolve a1s)
    | Ast1_stmt_empty ->
        Ast1_stmt_empty
    | Ast1_fbind (a1,a2) ->
        Ast1_fbind (a1, resolve a2)
    | Ast1_pat a1 ->
        Ast1_pat (resolve a1)
    (* Similar key cases for patterns *)
    | Ast1_infixpat_op _
    | Ast1_infixpat_pat10 _ ->
        (do_infixpat (None,i) ast).node1
    | Ast1_pat10_con (a1,a2s) ->
        Ast1_pat10_con (a1, List.map resolve a2s)
    | Ast1_pat10_apat a1 ->
        Ast1_pat10_apat (resolve a1)
    | Ast1_apat_var a1 ->
        Ast1_apat_var a1
    | Ast1_apat_as (a1,a2) ->
        Ast1_apat_as (a1, resolve a2)
    | Ast1_apat_con a1 ->
        Ast1_apat_con a1
    | Ast1_apat_lbpat (a1,a2s) ->
        Ast1_apat_lbpat (a1, List.map resolve a2s)
    | Ast1_apat_literal a1 ->
        Ast1_apat_literal a1
    | Ast1_apat_wild ->
        Ast1_apat_wild
    | Ast1_apat_paren a1 ->
        Ast1_apat_paren (resolve a1)
    | Ast1_apat_tuple a1s ->
        Ast1_apat_tuple (List.map resolve a1s)
    | Ast1_apat_list a1s ->
        Ast1_apat_list (List.map resolve a1s)
    | Ast1_apat_irref a1 ->
        Ast1_apat_irref (resolve a1)
    | Ast1_fpat (a1,a2) ->
        Ast1_fpat (a1, resolve a2)
    | _ -> assert false
  in { ast with node1 = newnode }
(* The two functions below do all the actual work. They are identical except
 * that one processes expressions, and the other processes patterns.
 * do_infix[exp|pat] : int -> associativity -> ast <Ast1_infix*>
 *   -> ast <Ast1_infix*>
 * The given integer (i) is a lower bound on what operators are
 * permitted in the given infixexp - any operators in the
 * given exp/pat with precedence < i are errors.
 * If associativity is Non, operators of precedence == i are permitted
 * anywhere. However, if associativity is Left or Right, we have an implied
 * precedence i operator directly to the right (if assoc==Left)
 * or left (if assoc==Right) of the given expression. That is, in order to
 * avoid an error, the only precedence == i operators permitted are those of
 * the same associativity located on the appropriate end of the expression. *)
and do_infixexp min asc ast =


