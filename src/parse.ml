open Batteries
;;
open Types
;;

module Haskell_parser_sim = Parser_gen.Make_sim (
  struct
    type tm = token
    type ntm = nonterm
    type lx = lexeme
    let lx_to_tm = (fun lx -> lx.token )
    let eof = EOF
    type ast = ast0
    let lx_print = Print.lexeme_print
    let ast_print = Print.ast0_print
  end
)

open Haskell_parser_sim
;;

(* Some simple helper functions for our semantic actions. *)

(* Find the start and end block bounds of a given ast node, given those for all
 * its children. Return the parent ast node with the bounds filled in *)
let do_bounds child_asts parent_astnode =
  let min_start = List.min (List.map (fun ast -> ast.blockstart) child_asts)
  and max_end   = List.max (List.map (fun ast -> ast.blockend) child_asts)
  in { node = parent_astnode; blockstart = min_start; blockend = max_end; }
;;

(* Cons a given ast onto an Ast0_partial_list. Return the new ast0node. *)
let ast0cons car_ast cdr_ast =
  match cdr_ast.node with
  | Ast0_partial_list cdr_asts ->
      Ast0_partial_list (car_ast::cdr_asts)
  | _ -> assert false
;;

(* Extract the list from a given Ast0_partial_list *)
let ast0getlist ast =
  match ast.node with
  | Ast0_partial_list asts -> asts
  | _ -> assert false
;;

(* A few boilerplate semantic actions that we use repeatedly *)

(* Build a partial list by consing the 0th input ast onto the 2nd (assuming the
 * 1st is a delimiter *)
let cons_action asts =
  let car_ast = List.hd asts
  and cdr_ast = List.at asts 2 in
  do_bounds asts (ast0cons car_ast cdr_ast)
;;

(* Same, but with 1st as cdr *)
let cons_nodelim_action asts =
  let car_ast = List.hd asts
  and cdr_ast = List.at asts 1 in
  do_bounds asts (ast0cons car_ast cdr_ast)
;;

(* Build a one-element partial list from the 0th input ast *)
let singleton_action asts =
  do_bounds asts (Ast0_partial_list [List.hd asts])
;;

(* Extract the nth input ast and ignore the rest (except for bounds) *)
let at_action n asts =
  do_bounds asts ((List.at asts n).node)
;;

(* Extract the 0th input ast, leaving its bounds unmodified. Same as (at_action
 * 0) in case where there is only one input ast - the intended use case.  *)
let id_action asts =
  List.hd asts
;;

(* CHANGES TO THE HASKELL 98 GRAMMAR:
 * No datatype contexts
 * https://ghc.haskell.org/trac/haskell-prime/wiki/NoDatatypeContexts
 * No n+k patterns (made in Haskell 2010)
 * https://ghc.haskell.org/trac/haskell-prime/wiki/NoNPlusKPatterns
 * Fixity resolution is later (made in Haskell 2010). We also handle unary
 * negation at that point.
 * https://ghc.haskell.org/trac/haskell-prime/wiki/FixityResolution *)

(* We also don't handle strict (!) fields, because having special grammar
 * symbols for things that are neither reserved operators nor special
 * characters is a stupid idea and makes parsing awful.  as_kwd implemented in
 * Parser_gen, we resolve shift-reduce conflicts by shifting (so lambda, let,
 * if, etc. extend as_kwd far to the right as possible). *)

(* The empty program (no tokens) does not compile.
 * To eliminate reduce-reduce conflicts:
 * * patterns parsed as_kwd expressions
 * * function / pattern left-hand sides of expressions parsed as_kwd general
 *     expressions (no distinguished variable / op)
 * * vars parsed as_kwd qvars
 * * import/export decls do not distinguish classes from data constructors
 * * contexts parsed as_kwd types
 * * import decls parsed as_kwd top-level decls *)
let haskell_acfg = {

  (* There are only a few things we do in semantic actions. We grab the child
   * AST nodes that have semantic meaning and put them in the AST we
   * are building, and we compute the appropriate source code bounds for that
   * portion of the AST. Note that our terminal action builds an AST node for
   * *every* syntactic terminal - including terminals that are useless once
   * parsing is complete, such as_kwd puncuation. This is why we only ever use
   * *some* of the child nodes in each semantic action. *)

  (* Furthermore, we occasionally we copy the contents of a
   * single child up (in cases where the AST node could only have one child
   * anyways. eg a body always consists of a topdecllist, and hence the
   * Ast0_body node type simply has_kwd those topdecls as its (direct) children
   * so when we encounter a NTbody we need to copy the children of the
   * Ast0_topdecllist node into the Ast0_body node. See below. *)

  goal = Goal;
  productions =
    [| { lhs = Goal;
         rhs = [ NT NTmodule ];
         semantic_action = id_action
       };
       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; T RWhere; NT NTbody ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1
             and body = List.at asts 3 in
             do_bounds asts (Ast0_module (Some modid, None, body)));
       };
       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; NT NTexports; T RWhere; NT NTbody ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1
             and exports = List.at asts 3
             and body = List.at asts 4 in
             do_bounds asts (Ast0_module (Some modid, Some
               (ast0getlist exports), body)))
       };
       { lhs = NTmodule;
         rhs = [ NT NTbody ];
         semantic_action =
           (fun asts ->
             let body = List.at asts 0 in
             do_bounds asts (Ast0_module (None, None, body)));
       };
       { lhs = NTbody;
         rhs = [ T LCurly; NT NTtopdecls; T RCurly ];
         semantic_action =
           (fun asts ->
             let topdecls = List.at asts 1 in
               do_bounds asts
                 (Ast0_body (ast0getlist topdecls)))
       };
       { lhs = NTexports;
         rhs = [ T LParen; T RParen ];
         semantic_action =
           (fun asts -> do_bounds asts (Ast0_partial_list []))
       };
       { lhs = NTexports;
         rhs = [ T LParen; T Comma; T RParen ];
         semantic_action =
           (fun asts -> do_bounds asts (Ast0_partial_list []))
       };
       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T RParen ];
         semantic_action =
           (fun asts ->
             let exportlist = List.at asts 1 in
             do_bounds asts exportlist.node)
       };
       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T Comma; T RParen ];
         semantic_action =
           (fun asts ->
             let exportlist = List.at asts 1 in
             do_bounds asts exportlist.node)
       };
       { lhs = NTexportlist;
         rhs = [ NT NTexport; T Comma; NT NTexportlist ];
         semantic_action = cons_action
       };
       { lhs = NTexportlist;
         rhs = [ NT NTexport ];
         semantic_action = singleton_action
       };
       { lhs = NTexport;
         rhs = [ NT NTqvar ];
         semantic_action =
           (fun asts ->
             let qvar = List.hd asts in
             do_bounds asts (Ast0_export_var qvar))
       };
       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; T RDotDot; T RParen ];
         semantic_action =
           (fun asts ->
             let qtycon = List.hd asts in
             do_bounds asts (Ast0_export_type (qtycon, None)))
       };
       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; T RParen ];
         semantic_action =
           (fun asts ->
             let qtycon = List.hd asts in
             do_bounds asts (Ast0_export_type (qtycon, Some [])))
       };
       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; NT NTqcnamelist; T RParen ];
         semantic_action =
           (fun asts ->
             let qconid = List.hd asts
             and qcnamelist = List.at asts 2 in
             do_bounds asts (Ast0_export_type (qconid,
               Some (ast0getlist qcnamelist))))
       };
       { lhs = NTexport;
         rhs = [ T RModule; T ConId ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1 in
             do_bounds asts (Ast0_export_module modid))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; T VarId; T ConId; NT NTimpspec ];
         semantic_action =
           (fun asts ->
             let qualified = List.at asts 1
             and modid = List.at asts 2
             and as_kwd = List.at asts 3
             and asmodid = List.at asts 4
             and impspec = List.at asts 5 in
             do_bounds asts (Ast0_topdecl_import (Some qualified,
               modid, Some as_kwd, Some asmodid, Some impspec)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; T VarId; T ConId ];
         semantic_action =
           (fun asts ->
             let qualified = List.at asts 1
             and modid = List.at asts 2
             and as_kwd = List.at asts 3
             and asmodid = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_import (Some qualified,
               modid, Some as_kwd, Some asmodid, None)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; NT NTimpspec ];
         semantic_action =
           (fun asts ->
             let qualified = List.at asts 1
             and modid = List.at asts 2
             and impspec = List.at asts 3 in
             do_bounds asts (Ast0_topdecl_import (Some qualified,
               modid, None, None, Some impspec)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId ];
         semantic_action =
           (fun asts ->
             let qualified = List.at asts 1
             and modid = List.at asts 2 in
             do_bounds asts (Ast0_topdecl_import (Some qualified,
               modid, None, None, None)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; T VarId; T ConId; NT NTimpspec ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1
             and as_kwd = List.at asts 2
             and asmodid = List.at asts 3
             and impspec = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_import (None,
               modid, Some as_kwd, Some asmodid, Some impspec)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; T VarId; T ConId ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1
             and as_kwd = List.at asts 2
             and asmodid = List.at asts 3 in
             do_bounds asts (Ast0_topdecl_import (None,
               modid, Some as_kwd, Some asmodid, None)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; NT NTimpspec ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1
             and impspec = List.at asts 2 in
             do_bounds asts (Ast0_topdecl_import (None,
               modid, None, None, Some impspec)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId ];
         semantic_action =
           (fun asts ->
             let modid = List.at asts 1 in
             do_bounds asts (Ast0_topdecl_import (None,
               modid, None, None, None)))
       };
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T RParen ];
         semantic_action =
           (fun asts ->
             let hiding = List.hd asts in
             do_bounds asts (Ast0_impspec (Some hiding, [])))
       };
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T Comma; T RParen ];
         semantic_action =
           (fun asts ->
             let hiding = List.hd asts in
             do_bounds asts (Ast0_impspec (Some hiding, [])))
       };
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T RParen ];
         semantic_action =
           (fun asts ->
             let hiding = List.hd asts
             and importlist = List.at asts 2 in
             do_bounds asts (Ast0_impspec (Some hiding,
               ast0getlist importlist)))
       };
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T Comma; T RParen ];
         semantic_action =
           (fun asts ->
             let hiding = List.hd asts
             and importlist = List.at asts 2 in
             do_bounds asts (Ast0_impspec (Some hiding,
               ast0getlist importlist)))
       };
       { lhs = NTimpspec;
         rhs = [ T LParen; T RParen ];
         semantic_action =
           (fun asts ->
             do_bounds asts (Ast0_impspec (None, [])))
       };
       { lhs = NTimpspec;
         rhs = [ T LParen; T Comma; T RParen ];
         semantic_action =
           (fun asts ->
             do_bounds asts (Ast0_impspec (None, [])))
       };
       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T RParen ];
         semantic_action =
           (fun asts ->
             let importlist = List.at asts 1 in
             do_bounds asts (Ast0_impspec (None,
               ast0getlist importlist)))
       };
       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T Comma; T RParen ];
         semantic_action =
           (fun asts ->
             let importlist = List.at asts 1 in
             do_bounds asts (Ast0_impspec (None,
               ast0getlist importlist)))
       };
       { lhs = NTimportlist;
         rhs = [ NT NTimport; T Comma; NT NTimportlist ];
         semantic_action = cons_action
       };
       { lhs = NTimportlist;
         rhs = [ NT NTimport ];
         semantic_action = singleton_action
       };
       { lhs = NTimport;
         rhs = [ NT NTqvar ];
         semantic_action =
           (fun asts ->
             let qvar = List.hd asts in
             do_bounds asts (Ast0_import_var qvar))
       };
       { lhs = NTimport;
         rhs = [ T ConId; T LParen; T RDotDot; T RParen ];
         semantic_action =
           (fun asts ->
             let conid = List.hd asts in
             do_bounds asts (Ast0_import_type (conid, None)))
       };
       { lhs = NTimport;
         rhs = [ T ConId; T LParen; T RParen ];
         semantic_action =
           (fun asts ->
             let conid = List.hd asts in
             do_bounds asts (Ast0_import_type (conid, Some [])))
       };
       { lhs = NTimport;
         rhs = [ T ConId; T LParen; NT NTqcnamelist; T RParen ];
         semantic_action =
           (fun asts ->
             let conid = List.hd asts
             and qcnamelist = List.at asts 2 in
             do_bounds asts (Ast0_import_type (conid, Some (ast0getlist
               qcnamelist))))
       };
       { lhs = NTqcname;
         rhs = [ NT NTqvar ];
         semantic_action = id_action
       };
       { lhs = NTqcname;
         rhs = [ NT NTqcon ];
         semantic_action = id_action
       };
       { lhs = NTtopdecls;
         rhs = [ NT NTtopdecllist ];
         semantic_action = id_action
       };
       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl; T Semicolon; NT NTtopdecllist ];
         semantic_action = cons_action
       };
       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl ];
         semantic_action = singleton_action
       };
       { lhs = NTtopdecl;
         rhs = [ T RType; NT NTsimpletype; T REquals; NT NTtype ];
         semantic_action =
           (fun asts ->
             let lhs = List.at asts 1
             and rhs = List.at asts 3 in
             do_bounds asts (Ast0_topdecl_type (lhs, rhs)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs; NT NTderiving ];
         semantic_action =
           (fun asts ->
             let lhs = List.at asts 1
             and rhs = List.at asts 3
             and deriving = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_data (lhs, ast0getlist rhs,
               Some deriving)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs ];
         semantic_action =
           (fun asts ->
             let lhs = List.at asts 1
             and rhs = List.at asts 3 in
             do_bounds asts (Ast0_topdecl_data (lhs, ast0getlist rhs,
               None)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr; NT NTderiving ];
         semantic_action =
           (fun asts ->
             let lhs = List.at asts 1
             and rhs = List.at asts 3
             and deriving = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_newtype (lhs, rhs,
               Some deriving)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr ];
         semantic_action =
           (fun asts ->
             let lhs = List.at asts 1
             and rhs = List.at asts 3 in
             do_bounds asts (Ast0_topdecl_newtype (lhs, rhs, None)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; T ConId; T VarId; T RWhere; NT NTdecls ];
         semantic_action =
           (fun asts ->
             let scontext = List.at asts 1
             and lhs_class = List.at asts 3
             and lhs_var = List.at asts 4
             and rhs = List.at asts 6 in
             do_bounds asts (Ast0_topdecl_class (Some scontext,
               lhs_class, lhs_var, ast0getlist rhs)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; T ConId; T VarId ];
         semantic_action =
           (fun asts ->
             let scontext = List.at asts 1
             and lhs_class = List.at asts 3
             and lhs_var = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_class (Some scontext,
               lhs_class, lhs_var, [])))
       };
       { lhs = NTtopdecl;
         rhs = [ T RClass; T ConId; T VarId; T RWhere; NT NTdecls ];
         semantic_action =
           (fun asts ->
             let lhs_class = List.at asts 1
             and lhs_var = List.at asts 2
             and rhs = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_class (None,
               lhs_class, lhs_var, ast0getlist rhs)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RClass; T ConId; T VarId ];
         semantic_action =
           (fun asts ->
             let lhs_class = List.at asts 1
             and lhs_var = List.at asts 2 in
             do_bounds asts (Ast0_topdecl_class (None,
               lhs_class, lhs_var, [])))
       };
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqconid; NT NTinst; T RWhere; NT NTdecls ];
         semantic_action =
           (fun asts ->
             let scontext = List.at asts 1
             and lhs_class = List.at asts 3
             and lhs_inst = List.at asts 4
             and rhs = List.at asts 6 in
             do_bounds asts (Ast0_topdecl_instance (Some scontext,
               lhs_class, lhs_inst, ast0getlist rhs)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqconid; NT NTinst ];
         semantic_action =
           (fun asts ->
             let scontext = List.at asts 1
             and lhs_class = List.at asts 3
             and lhs_inst = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_instance (Some scontext,
               lhs_class, lhs_inst, [])))
       };
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqconid; NT NTinst; T RWhere; NT NTdecls ];
         semantic_action =
           (fun asts ->
             let lhs_class = List.at asts 1
             and lhs_inst = List.at asts 2
             and rhs = List.at asts 4 in
             do_bounds asts (Ast0_topdecl_instance (None,
               lhs_class, lhs_inst, ast0getlist rhs)))
       };
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqconid; NT NTinst ];
         semantic_action =
           (fun asts ->
             let lhs_class = List.at asts 1
             and lhs_inst = List.at asts 2 in
             do_bounds asts (Ast0_topdecl_instance (None,
               lhs_class, lhs_inst, [])))
       };
       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; T RParen ];
         semantic_action =
           (fun asts ->
             do_bounds asts (Ast0_topdecl_default []))
       };
       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; NT NTtypelist; T RParen ];
         semantic_action =
           (fun asts ->
             let typelist = List.at asts 2 in
             do_bounds asts (Ast0_topdecl_default (ast0getlist typelist)))
       };
       { lhs = NTtopdecl;
         rhs = [ NT NTdecl ];
         semantic_action =
           (fun asts ->
             let decl = List.hd asts in
             do_bounds asts (Ast0_topdecl_decl decl))
       };
       { lhs = NTtypelist;
         rhs = [ NT NTtype; T Comma; NT NTtypelist ];
         semantic_action = cons_action
       };
       { lhs = NTtypelist;
         rhs = [ NT NTtype ];
         semantic_action = singleton_action
       };
       { lhs = NTdecls;
         rhs = [ T LCurly; T RCurly ];
         semantic_action =
           (fun asts ->
             do_bounds asts (Ast0_partial_list []))
       };
       { lhs = NTdecls;
         rhs = [ T LCurly; NT NTdecllist; T RCurly ];
         semantic_action = at_action 1
       };
       { lhs = NTdecllist;
         rhs = [ NT NTdecl; T Semicolon; NT NTdecllist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdecllist;
         rhs = [ NT NTdecl ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdecl;
         rhs = [ NT NTgendecl ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdecl;
         rhs = [ NT NTinfixexp; NT NTrhs ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgendecl;
         rhs = [];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgendecl;
         rhs = [ NT NTqvars; T RColonColon; NT NTtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgendecl;
         rhs = [ NT NTfixity; T IntLit; NT NTops ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgendecl;
         rhs = [ NT NTfixity; NT NTops ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTops;
         rhs = [ NT NToplist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NToplist;
         rhs = [ NT NTop; T Comma; NT NToplist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NToplist;
         rhs = [ NT NTop ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvars;
         rhs = [ NT NTqvarlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarlist;
         rhs = [ NT NTqvar; T Comma; NT NTqvarlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarlist;
         rhs = [ NT NTqvar ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfixity;
         rhs = [ T RInfixl ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfixity;
         rhs = [ T RInfixr ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfixity;
         rhs = [ T RInfix ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtype;
         rhs = [ NT NTbtype; T RDashRArrow; NT NTtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtype;
         rhs = [ NT NTbtype; T REqualsRArrow; NT NTtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtype;
         rhs = [ NT NTbtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTbtype;
         rhs = [ NT NTbtype; NT NTatype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTbtype;
         rhs = [ NT NTatype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTatype;
         rhs = [ NT NTgtycon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTatype;
         rhs = [ T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T Comma; NT NTtypelist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTatype;
         rhs = [ T LSquare; NT NTtype; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgtycon;
         rhs = [ NT NTqconid ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgtycon;
         rhs = [ T LParen; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgtycon;
         rhs = [ T LSquare; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgtycon;
         rhs = [ T LParen; T RDashRArrow; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgtycon;
         rhs = [ T LParen; NT NTcommalist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTcommalist;
         rhs = [ T Comma; NT NTcommalist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTcommalist;
         rhs = [ T Comma ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTscontext;
         rhs = [ NT NTsimpleclass ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTscontext;
         rhs = [ T LParen; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTscontext;
         rhs = [ T LParen; NT NTsimpleclasslist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass; T Comma; NT NTsimpleclasslist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTsimpleclass;
         rhs = [ NT NTqconid; T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTsimpletype;
         rhs = [ T ConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTsimpletype;
         rhs = [ T ConId; NT NTtyvarlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtyvarlist;
         rhs = [ T VarId; NT NTtyvarlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtyvarlist;
         rhs = [ T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstrs;
         rhs = [ NT NTconstrlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstrlist;
         rhs = [ NT NTconstr; T RPipe; NT NTconstrlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstrlist;
         rhs = [ NT NTconstr ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstr;
         rhs = [ NT NTbtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstr;
         rhs = [ NT NTbtype; NT NTconop; NT NTbtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; NT NTfielddecllist; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl; T Comma; NT NTfielddecllist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTnewconstr;
         rhs = [ NT NTcon; NT NTatype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTnewconstr;
         rhs = [ NT NTcon; T LCurly; NT NTqvar; T RColonColon; NT NTtype; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfielddecl;
         rhs = [ NT NTqvars; T RColonColon; NT NTtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTderiving;
         rhs = [ T RDeriving; NT NTdclass ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; NT NTdclasslist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdclasslist;
         rhs = [ NT NTdclass; T Comma; NT NTdclasslist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdclasslist;
         rhs = [ NT NTdclass ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTdclass;
         rhs = [ NT NTqconid ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ NT NTgtycon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; NT NTtyvarlist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ T LParen; T VarId; T Comma; NT NTtyvarcommalist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ T LSquare; T VarId; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinst;
         rhs = [ T LParen; T VarId; T RDashRArrow; T VarId; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtyvarcommalist;
         rhs = [ T VarId; T Comma; NT NTtyvarcommalist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTtyvarcommalist;
         rhs = [ T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexplist;
         rhs = [ NT NTaexp; NT NTaexplist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexplist;
         rhs = [ NT NTaexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp; T RWhere; NT NTdecls ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTrhs;
         rhs = [ NT NTgdrhs; T RWhere; NT NTdecls ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTrhs;
         rhs = [ NT NTgdrhs ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp; NT NTgdrhs ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgd;
         rhs = [ T RPipe; NT NTinfixexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp;
         rhs = [ NT NTinfixexp; T RColonColon; NT NTtype ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp;
         rhs = [ NT NTinfixexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinfixexp;
         rhs = [ NT NTexp10; NT NTqop; NT NTinfixexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTinfixexp;
         rhs = [ NT NTexp10 ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ T RBackslash; NT NTaexplist; T RDashRArrow; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ T RLet; NT NTdecls; T RIn; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ T RIf; NT NTexp; T RThen; NT NTexp; T RElse; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ T RCase; NT NTexp; T ROf; T LCurly; NT NTalts; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ T RDo; T LCurly; NT NTstmts; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexp10;
         rhs = [ NT NTaexplist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTqvar ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTgcon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTliteral ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T Comma; NT NTexplist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexplist; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; NT NTexp; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; NT NTexp; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RPipe; NT NTquallist; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTinfixexp; NT NTqop; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTqop; NT NTinfixexp; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTqcon; T LCurly; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTqcon; T LCurly; NT NTfbindlist; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTaexp; T LCurly; NT NTfbindlist; T RCurly ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ NT NTqvar; T RAt; NT NTaexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T RTilde; NT NTaexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaexp;
         rhs = [ T RUnderscore ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexplist;
         rhs = [ NT NTexp; T Comma; NT NTexplist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTexplist;
         rhs = [ NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTquallist;
         rhs = [ NT NTqual; T Comma; NT NTquallist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTquallist;
         rhs = [ NT NTqual ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfbindlist;
         rhs = [ NT NTfbind; T Comma; NT NTfbindlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfbindlist;
         rhs = [ NT NTfbind ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqual;
         rhs = [ NT NTexp; T RLArrowDash; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqual;
         rhs = [ T RLet; NT NTdecls ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqual;
         rhs = [ NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTalts;
         rhs = [ NT NTaltlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaltlist;
         rhs = [ NT NTalt; T Semicolon; NT NTaltlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTaltlist;
         rhs = [ NT NTalt ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTalt;
         rhs = [ NT NTexp; T RDashRArrow; NT NTexp; T RWhere; NT NTdecls ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTalt;
         rhs = [ NT NTexp; T RDashRArrow; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTalt;
         rhs = [ NT NTexp; NT NTgdpat; T RWhere; NT NTdecls ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTalt;
         rhs = [ NT NTexp; NT NTgdpat ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp; NT NTgdpat ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmts;
         rhs = [ NT NTstmtlist; NT NTexp; T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmts;
         rhs = [ NT NTstmtlist; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmts;
         rhs = [ NT NTexp; T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmts;
         rhs = [ NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmtlist;
         rhs = [ NT NTstmt; T Semicolon; NT NTstmtlist ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmtlist;
         rhs = [ NT NTstmt ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmt;
         rhs = [ NT NTexp; T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmt;
         rhs = [ NT NTexp; T RLArrowDash; NT NTexp; T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmt;
         rhs = [ T RLet; NT NTdecls; T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTstmt;
         rhs = [ T Semicolon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTfbind;
         rhs = [ NT NTqvar; T REquals; NT NTexp ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgcon;
         rhs = [ T LParen; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgcon;
         rhs = [ T LSquare; T RSquare ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgcon;
         rhs = [ T LParen; NT NTcommalist; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTgcon;
         rhs = [ NT NTqcon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvar;
         rhs = [ T QVarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvar;
         rhs = [ T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvar;
         rhs = [ T LParen; T QVarSym; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvar;
         rhs = [ T LParen; T VarSym; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTcon;
         rhs = [ T ConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTcon;
         rhs = [ T LParen; T ConSym; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqcon;
         rhs = [ T QConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqcon;
         rhs = [ T ConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqcon;
         rhs = [ T LParen; T RColon; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqcon;
         rhs = [ T LParen; T QConSym; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqcon;
         rhs = [ T LParen; T ConSym; T RParen ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarop;
         rhs = [ T QVarSym ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarop;
         rhs = [ T VarSym ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarop;
         rhs = [ T Backquote; T QVarId; T Backquote ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarop;
         rhs = [ T Backquote; T VarId; T Backquote ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconop;
         rhs = [ T ConSym ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTconop;
         rhs = [ T Backquote; T ConId; T Backquote ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconop;
         rhs = [ T RColon ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconop;
         rhs = [ T QConSym ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconop;
         rhs = [ T ConSym ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconop;
         rhs = [ T Backquote; T QConId; T Backquote ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconop;
         rhs = [ T Backquote; T ConId; T Backquote ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTop;
         rhs = [ NT NTqvarop ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTop;
         rhs = [ NT NTconop ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqop;
         rhs = [ NT NTqvarop ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqop;
         rhs = [ NT NTqconop ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarid;
         rhs = [ T QVarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqvarid;
         rhs = [ T VarId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconid;
         rhs = [ T QConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTqconid;
         rhs = [ T ConId ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTliteral;
         rhs = [ T IntLit ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTliteral;
         rhs = [ T FloatLit ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTliteral;
         rhs = [ T CharLit ];
         semantic_action =
           (fun _ -> 0);
       };
       { lhs = NTliteral;
         rhs = [ T StringLit ];
         semantic_action =
           (fun _ -> 0);
       };
    |];
  terminal_action = (fun lx ->
    { node = Ast0_leaf lx; blockstart = lx.startraw; blockend = lx.endraw });
}

let parse lxq =
  match simulate haskell_acfg Computed_actions_gotos.computed_do_action
    Computed_actions_gotos.computed_do_goto
    (fun lx -> failwith
      (Printf.sprintf2 "Syntax error at %a" Print.lexeme_print lx))
    lxq with
  | 0 -> { node = Ast0_gcon_tuple 1; blockstart = -1; blockend = -1 }
  | _ -> { node = Ast0_gcon_tuple 0; blockstart = -1; blockend = -1 }
;;
