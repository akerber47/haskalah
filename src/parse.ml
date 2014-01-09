open Batteries
;;

(* Comment above each AST node says what sort of children it *should* have. No
 * type system guarantees. *)
type ast0 = {
  node : ast0node;
  (* For error reporting purposes, start and end of this syntax block in
   * source. Basically just min/max over all tokens that make up the block.
   * (ignoring implicitly generated tokens) *)
  startraw : int;
  endraw   : int;
}
and ast0node =
  (* modid [export*] body *)
  [ `Module of ast0 * (ast0 list) option * ast0
  (* impdecl* topdecl* *)
  | `Body of (ast0 list) * (ast0 list)
  (* qvar *)
  | `Export_var of ast0
  (* qtycon [cname*] *)
  | `Export_type of ast0 * (ast0 list) option
  (* qtycls [qvar*] *)
  | `Export_class of ast0 * (ast0 list) option
  (* modid *)
  | `Export_module of ast0
  (* qualified? modid [modid] hiding? import* *)
  | `Impdecl of bool * ast0 * (ast0 option) * bool * (ast0 list)
  (* var *)
  | `Import_var of ast0
  (* tycon [cname*] *)
  | `Import_type of ast0 * (ast0 list) option
  (* tycls [var*] *)
  | `Import_class of ast0 * (ast0 list) option
  (* var *)
  | `Cname_var of ast0
  (* con *)
  | `Cname_con of ast0
  (* simpletype type *)
  | `Topdecl_type of ast0 * ast0
  (* [context] simpletype constr* [deriving] *)
  | `Topdecl_data of ast0 option * ast0 * ast0 list * ast0 option
  (* [context] simpletype newconstr [deriving] *)
  | `Topdecl_newtype of ast0 option * ast0 * ast0 * ast0 option
  (* [scontext] tycls tyvar cdecl* *)
  | `Topdecl_class of ast0 option * ast0 * ast0 * ast0 list
  (* [scontext] qtycls inst idecl* *)
  | `Topdecl_instance of ast0 option * ast0 * ast0 * ast0 list
  (* type* *)
  | `Topdecl_default of ast0 list
  (* decl *)
  | `Topdecl_decl of ast0
  (* gendecl *)
  | `Decl_general of ast0
  (* funlhs rhs *)
  | `Decl_fun of ast0 * ast0
  (* infixpat rhs *)
  | `Decl_pat of ast0 * ast0
  (* gendecl *)
  | `Cdecl_general of ast0
  (* funlhs rhs *)
  | `Cdecl_fun of ast0 * ast0
  (* var rhs *)
  | `Cdecl_var of ast0 * ast0
  (* funlhs rhs *)
  | `Idecl_fun of ast0 * ast0
  (* var rhs *)
  | `Idecl_var of ast0 * ast0
  (* *)
  | `Idecl_empty
  (* var* [context] type *)
  | `Gendecl_type of ast0 list * ast0 option * ast0
  (* fixity [integer] [op] *)
  | `Gendecl_fixity of ast0 * ast0 option * ast0 list
  (* *)
  | `Gendecl_empty
  | `Fixity_left
  | `Fixity_right
  | `Fixity_none
  (* btype [atype] *)
  | `Type of ast0 * ast0 option
  (* [btype] atype *)
  | `Btype of ast0 option * ast0
  (* gtycon *)
  | `Atype_con of ast0
  (* tyvar *)
  | `Atype_var of ast0
  (* type* *)
  | `Atype_tuple of ast0 list
  (* type *)
  | `Atype_list of ast0
  (* type *)
  | `Atype_paren of ast0
  (* qtycon *)
  | `Gtycon_con of ast0
  | `Gtycon_unit
  | `Gtycon_list
  | `Gtycon_fun
  | `Gtycon_tuple of int
  (* class* *)
  | `Context of ast0 list
  (* qtycls tyvar *)
  | `Class_simple of ast0 * ast0
  (* qtycls tyvar atype* *)
  | `Class_complex of ast0 * ast0 * ast0 list
  (* simpleclass* *)
  | `Scontext of ast0 list
  (* qtycls tyvar *)
  | `Simpleclass of ast0 * ast0
  (* tycon tyvar* *)
  | `Simpletype of ast0 * ast0 list
  (* con (atype, strict?)* *)
  | `Constr_con of ast0 * (ast0 * bool) list
  (* btype/atype strict? conop btype/atype strict? *)
  | `Constr_conop of ast0 * bool * ast0 * ast0 * bool
  (* con fielddecl* *)
  | `Constr_fields of ast0 * ast0 list
  (* con atype *)
  | `Newconstr_con of ast0
  (* con var type *)
  | `Newconstr_field of ast0 * ast0 * ast0
  (* var* type/atype strict? *)
  | `Fielddecl of ast0 list * ast0 * bool
  (* dclass* *)
  | `Deriving of ast0 list
  (* qtycls *)
  | `Dclass of ast0
  (* gtycon *)
  | `Inst_con of ast0
  (* gtycon tyvar* *)
  | `Inst_app of ast0 * ast0 list
  (* tyvar* *)
  | `Inst_tuple of ast0 list
  (* tyvar *)
  | `Inst_list of ast0
  (* tyvar tyvar *)
  | `Inst_fun of ast0 * ast0
  (* var apat* *)
  | `Funlhs_var of ast0 * ast0 list
  (* infixpat *)
  | `Funlhs_pat of ast0
  (* funlhs apat* *)
  | `Funlhs_app of ast0
  (* exp [decl*] *)
  | `Rhs_exp of ast0 * (ast0 list) option
  (* gdrhs [decl*] *)
  | `Rhs_guard of ast0 * (ast0 list) option
  (* gd exp [gdrhs] *)
  | `Gdrhs of ast0 * ast0 * (ast0 list) option
  (* infixexp *)
  | `Gd of ast0
  (* infixexp [context] type *)
  | `Exp_typed of ast0 * ast0 option * ast0
  (* infixexp *)
  | `Exp_infix of ast0
  (* exp10 qop infixexp *)
  | `Infixexp_op of ast0 * ast0 * ast0
  (* infixexp *)
  | `Infixexp_negate of ast0
  (* exp10 *)
  | `Infixexp_exp10 of ast0
  (* apat* exp *)
  | `Exp10_lambda of ast0 list * ast0
  (* decl* exp *)
  | `Exp10_let of ast0 list * ast0
  (* exp exp exp *)
  | `Exp10_if of ast0 * ast0 * ast0
  (* exp alt* *)
  | `Exp10_case of ast0 * ast0 list
  (* stmt* *)
  | `Exp10_do of ast0 list
  (* fexp *)
  | `Exp10_exp of ast0
  (* [fexp] aexp *)
  | `Fexp of ast0 option * ast0
  (* qvar *)
  | `Aexp_var of ast0
  (* gcon *)
  | `Aexp_con of ast0
  (* literal *)
  | `Aexp_literal of ast0
  (* exp *)
  | `Aexp_paren of ast0
  (* exp* *)
  | `Aexp_tuple of ast0 list
  (* exp* *)
  | `Aexp_list of ast0 list
  (* exp [exp] [exp] *)
  | `Aexp_seq of ast0 * ast0 option * ast0 option
  (* exp qual* *)
  | `Aexp_comp of ast0 * ast0 list
  (* infixexp qop *)
  | `Aexp_lsec of ast0 * ast0
  (* qop infixexp *)
  | `Aexp_rsec of ast0 * ast0
  (* qop fbind* *)
  | `Aexp_labelcon of ast0 * ast0 list
  (* aexp fbind* *)
  | `Aexp_labelupdate of ast0 * ast0 list
  (* pat exp *)
  | `Qual_gen of ast0 * ast0
  (* decl* *)
  | `Qual_let of ast0 list
  (* exp *)
  | `Qual_guard of ast0
  (* pat exp [decl*] *)
  | `Alt_pat of ast0 * ast0 * (ast0 list) option
  (* pat gdpat [decl*] *)
  | `Alt_guard of ast0 * ast0 * (ast0 list) option
  (* gd exp [gdpat] *)
  | `Gdpat of ast0 * ast0 * ast0 option
  (* exp *)
  | `Stmt_exp of ast0
  (* pat exp *)
  | `Stmt_assign of ast0 * ast0
  (* decl* *)
  | `Stmt_let of ast0 list
  (* *)
  | `Stmt_empty
  (* qvar exp *)
  | `Fbind of ast0 * ast0
  (* NOTE no n+k patterns in our grammar *)
  (* infixpat *)
  | `Pat of ast0
  (* pat10 qconop infixpat *)
  | `Infixpat_op of ast0 * ast0 * ast0
  (* - (integer | float) *)
  | `Infixpat_negate of ast0
  (* pat10 *)
  | `Infixpat_pat10 of ast0
  (* apat *)
  | `Pat10_pat of ast0
  (* gcon apat* *)
  | `Pat10_conapp of ast0 * ast0 list
  (* var apat *)
  | `Apat_as of ast0 * (ast0 option)
  (* gcon *)
  | `Apat_nullary of ast0
  (* qcon fpat* *)
  | `Apat_labeled of ast0 * ast0 list
  (* literal *)
  | `Apat_literal of ast0
  (* *)
  | `Apat_wildcard
  (* pat *)
  | `Apat_paren of ast0
  (* pat* *)
  | `Apat_tuple of ast0 list
  (* pat* *)
  | `Apat_list of ast0 list
  (* pat *)
  | `Apat_irref of ast0
  (* qvar pat *)
  | `Fpat of ast0 * ast0
  (* *)
  | `Gcon_unit
  | `Gcon_list
  (* int *)
  | `Gcon_tuple of int
  (* qcon *)
  | `Gcon_qcon of ast0
  (* All of these below simply store a single id / symbol (or :), so
   * will be represented by leaf, with the lexeme storing the remaining
   * (actual) type data needed.
   * in lexical syntax:
   * varid, conid, tyvar, tyvar, tycon, tycls, modid, qvarid, qconid, qtycon,
   * qtycls, qvarsym, qconsym, literal.
   * in context-free syntax:
   * var, qvar, con, qcon, varop, qvarop, conop, qconop, op, qop, gconsym. *)
  | `Leaf of Lex.lexeme
  (* This AST node is produced by ALL the NT<stuff>list productions.
   * NT<stuff>list => <stuff>*
   * Basically this is a trick to cut down on how many AST nodes we need. *)
  | `Partial_list of ast0 list
  ]

(* Nonterminal symbols in the Haskell 98 grammar. Long, ugly verbatim copy of
 * section 9.5 of the Report. Note that we implement
 * https://ghc.haskell.org/trac/haskell-prime/wiki/FixityResolution ie resolve
 * precedence and fixities of operator expressions in a later step. This
 * violates the Haskell 98 specification. *)
type nonterm =
  | NTmodule
  | NTbody
  | NTimpdecls
  | NTexports
  | NTexport
  | NTimpdecl
  | NTimpspec
  | NTimport
  | NTcname
  | NTtopdecls
  | NTtopdecl
  | NTdecls
  | NTdecl
  | NTcdecls
  | NTcdecl
  | NTidecls
  | NTidecl
  | NTgendecl
  | NTops
  | NTvars
  | NTfixity
  | NTtype
  | NTbtype
  | NTatype
  | NTgtycon
  | NTcontext
  | NTclass
  | NTscontext
  | NTsimpleclass
  | NTsimpletype
  | NTconstrs
  | NTconstr
  | NTnewconstr
  | NTfielddecl
  | NTderiving
  | NTdclass
  | NTinst
  | NTfunlhs
  | NTrhs
  | NTgdrhs
  | NTgd
  | NTexp
  | NTinfixexp
  | NTexp10
  | NTfexp
  | NTaexp
  | NTqual
  | NTalts
  | NTalt
  | NTgdpat
  | NTstmts
  | NTstmt
  | NTfbind
  | NTpat
  | NTinfixpat
  | NTpat10
  | NTapat
  | NTfpat
  | NTgcon
  | NTvar
  | NTqvar
  | NTcon
  | NTqcon
  | NTvarop
  | NTqvarop
  | NTconop
  | NTqconop
  | NTop
  | NTqop
  | NTqconsym
  | NTimpdecllist
  | NTexportlist
  | NTcnamelist
  | NTqvarlist
  | NTimportlist
  | NTvarlist
  | NTtypelist
  | NTcommalist
  | NToplist
  | NTclasslist
  | NTsimpleclasslist
  | NTfielddecllist
  | NTdclasslist
  | NTexplist
  | NTquallist
  | NTfbindlist
  | NTfpatlist
  | NTpatlist
  | NTdecllist
  | NTcdecllist
  | NTidecllist
  | NTatypelist
  | NTtyvarlist
  | NTbangatypelist
  | NTapatlist

let nonterm_print o nt =
  let s =
    match nt with
    | NTmodule -> "module"
    | NTbody -> "body"
    | NTimpdecls -> "impdecls"
    | NTexports -> "exports"
    | NTexport -> "export"
    | NTimpdecl -> "impdecl"
    | NTimpspec -> "impspec"
    | NTimport -> "import"
    | NTcname -> "cname"
    | NTtopdecls -> "topdecls"
    | NTtopdecl -> "topdecl"
    | NTdecls -> "decls"
    | NTdecl -> "decl"
    | NTcdecls -> "cdecls"
    | NTcdecl -> "cdecl"
    | NTidecls -> "idecls"
    | NTidecl -> "idecl"
    | NTgendecl -> "gendecl"
    | NTops -> "ops"
    | NTvars -> "vars"
    | NTfixity -> "fixity"
    | NTtype -> "type"
    | NTbtype -> "btype"
    | NTatype -> "atype"
    | NTgtycon -> "gtycon"
    | NTcontext -> "context"
    | NTclass -> "class"
    | NTscontext -> "scontext"
    | NTsimpleclass -> "simpleclass"
    | NTsimpletype -> "simpletype"
    | NTconstrs -> "constrs"
    | NTconstr -> "constr"
    | NTnewconstr -> "newconstr"
    | NTfielddecl -> "fielddecl"
    | NTderiving -> "deriving"
    | NTdclass -> "dclass"
    | NTinst -> "inst"
    | NTfunlhs -> "funlhs"
    | NTrhs -> "rhs"
    | NTgdrhs -> "gdrhs"
    | NTgd -> "gd"
    | NTexp -> "exp"
    | NTinfixexp -> "infixexp"
    | NTexp10 -> "exp10"
    | NTfexp -> "fexp"
    | NTaexp -> "aexp"
    | NTqual -> "qual"
    | NTalts -> "alts"
    | NTalt -> "alt"
    | NTgdpat -> "gdpat"
    | NTstmts -> "stmts"
    | NTstmt -> "stmt"
    | NTfbind -> "fbind"
    | NTpat -> "pat"
    | NTinfixpat -> "infixpat"
    | NTpat10 -> "pat10"
    | NTapat -> "apat"
    | NTfpat -> "fpat"
    | NTgcon -> "gcon"
    | NTvar -> "var"
    | NTqvar -> "qvar"
    | NTcon -> "con"
    | NTqcon -> "qcon"
    | NTvarop -> "varop"
    | NTqvarop -> "qvarop"
    | NTconop -> "conop"
    | NTqconop -> "qconop"
    | NTop -> "op"
    | NTqop -> "qop"
    | NTqconsym -> "qconsym"
    | NTimpdecllist -> "impdecllist"
    | NTexportlist -> "exportlist"
    | NTcnamelist -> "cnamelist"
    | NTqvarlist -> "qvarlist"
    | NTimportlist -> "importlist"
    | NTvarlist -> "varlist"
    | NTtypelist -> "typelist"
    | NTcommalist -> "commalist"
    | NToplist -> "oplist"
    | NTclasslist -> "classlist"
    | NTsimpleclasslist -> "simpleclasslist"
    | NTfielddecllist -> "fielddecllist"
    | NTdclasslist -> "dclasslist"
    | NTexplist -> "explist"
    | NTquallist -> "quallist"
    | NTfbindlist -> "fbindlist"
    | NTfpatlist -> "fpatlist"
    | NTpatlist -> "patlist"
    | NTdecllist -> "decllist"
    | NTcdecllist -> "cdecllist"
    | NTidecllist -> "idecllist"
    | NTatypelist -> "atypelist"
    | NTtyvarlist -> "tyvarlist"
    | NTbangatypelist -> "bangatypelist"
    | NTapatlist -> "apatlist"
  in Printf.fprintf o "%s" s
;;

let ast0_print o _ =
  Printf.fprintf o "TODO" (* TODO *)
;;

module Haskell_parser_gen = Parser_gen.Make (
  struct
    type tm = Lex.token
    let tm_compare = compare
    type ntm = nonterm
    let ntm_compare = compare
    type lx = Lex.lexeme
    let lx_to_tm = (fun lx -> lx.Lex.token )
    let eof = Lex.EOF
    (* type ast = ast0 *)
    type ast = int
    let tm_print = Lex.token_print
    let ntm_print = nonterm_print
    let lx_print = Lex.lexeme_print
    (* let ast_print = ast0_print *)
    let ast_print = print_guess
  end
)

open Haskell_parser_gen
;;

let parse _ = { node = `Gcon_unit; startraw = -1; endraw = -1 }
;;

(* CHANGES TO THE HASKELL 98 GRAMMAR:
 * No datatype contexts
 * https://ghc.haskell.org/trac/haskell-prime/wiki/NoDatatypeContexts
 * No n+k patterns (made in Haskell 2010)
 * https://ghc.haskell.org/trac/haskell-prime/wiki/NoNPlusKPatterns
 * Fixity resolution is later (made in Haskell 2010). We also handle unary
 * negation at that point.
 * https://ghc.haskell.org/trac/haskell-prime/wiki/FixityResolution
 * We also don't handle strict fields, because having special grammar symbols
 * for things that are neither reserved operators nor special characters is a
 * stupid idea and makes parsing awful.
 * *)
let haskell_cfg = {
  goal = Goal;
  productions =
    [| { lhs = Goal;
         rhs = [NT NTmodule];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [T RModule; T ConId; T RWhere; NT NTbody];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [T RModule; T ConId; NT NTexports; T RWhere; NT NTbody];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [NTbody];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [T LCurly; NT NTimpdecls; T Semicolon;
          NT NTtopdecls; T RCurly];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [T LCurly; NT NTimpdecls; T RCurly];
         semantic_action = (fun _ -> 0);};

       { lhs = NT_NTbody;
         rhs = [ T_LCurly; NT_NTtopdecls; T_RCurly ];
       { lhs = NT_NTbody;
         rhs = [ T_LCurly; NT_NTtopdecls; T_RCurly ];
       { lhs = NT_NTimpdecls;
         rhs = [ NT_NTimpdecllist ];
       { lhs = NT_NTimpdecllist;
         rhs = [ NT_NTimpdecl; T_Semicolon; NT_NTimpdecllist ];
         rhs = [ NT_NTimpdecl ];
       { lhs = NT_NTexports;
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LParen; T_Comma; T_RParen ];
         rhs = [ T_LParen; NT_NTexportlist; T_RParen ];
         rhs = [ T_LParen; NT_NTexportlist; T_Comma; T_RParen ];
       { lhs = NT_NTexportlist;
         rhs = [ NT_NTexport; T_Comma; NT_NTexportlist ];
         rhs = [ NT_NTexport ];
       { lhs = NT_NTexport;
         rhs = [ NT_NTqvar ];
         rhs = [ NT_NTqtycon; T_LParen; T_RDotDot; T_RParen ];
         rhs = [ NT_NTqtycon; T_LParen; T_RParen ];
         rhs = [ NT_NTqtycon; T_LParen; NT_NTcnamelist; T_RParen ];
         rhs = [ NT_NTqtycls; T_LParen; T_RDotDot; T_RParen ];
         rhs = [ NT_NTqtycls; T_LParen; T_RParen ];
         rhs = [ NT_NTqtycls; T_LParen; NT_NTqvarlist; T_RParen ];
         rhs = [ T_RModule; NT_NTmodid ];
       { lhs = NT_NTimpdecl;
         rhs = [ T_RImport; T_RQualified; NT_NTmodid; T_RAs; NT_NTmodid; NT_NTimpspec ];
         rhs = [ T_RImport; T_RQualified; NT_NTmodid; T_RAs; NT_NTmodid ];
         rhs = [ T_RImport; T_RQualified; NT_NTmodid; NT_NTimpspec ];
         rhs = [ T_RImport; T_RQualified; NT_NTmodid ];
         rhs = [ T_RImport; NT_NTmodid; T_RAs; NT_NTmodid; NT_NTimpspec ];
         rhs = [ T_RImport; NT_NTmodid; T_RAs; NT_NTmodid ];
         rhs = [ T_RImport; NT_NTmodid; NT_NTimpspec ];
         rhs = [ T_RImport; NT_NTmodid ];
       { lhs = NT_NTimpspec;
         rhs = [ T_RHiding; T_LParen; T_RParen ];
         rhs = [ T_RHiding; T_LParen; T_Comma; T_RParen ];
         rhs = [ T_RHiding; T_LParen; NT_NTimportlist; T_RParen ];
         rhs = [ T_RHiding; T_LParen; NT_NTimportlist; T_Comma; T_RParen ];
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LParen; T_Comma; T_RParen ];
         rhs = [ T_LParen; NT_NTimportlist; T_RParen ];
         rhs = [ T_LParen; NT_NTimportlist; T_Comma; T_RParen ];
       { lhs = NT_NTimportlist;
         rhs = [ NT_NTimport; T_Comma; NT_NTimportlist ];
         rhs = [ NT_NTimport ];
       { lhs = NT_NTimport;
         rhs = [ NT_NTvar ];
         rhs = [ NT_NTtycon; T_LParen; T_RDotDot; T_RParen ];
         rhs = [ NT_NTtycon; T_LParen  T_RParen ];
         rhs = [ NT_NTtycon; T_LParen; NT_NTcnamelist; T_RParen ];
         rhs = [ NT_NTtycls; T_LParen; T_RDotDot; T_RParen ];
         rhs = [ NT_NTtycls; T_LParen; T_RParen ];
         rhs = [ NT_NTtycls; T_LParen; NT_NTvarlist; T_RParen ];
       { lhs = NT_NTcname;
         rhs = [ NT_NTvar ];
         rhs = [ NT_NTcon ];
       { lhs = NT_NTtopdecls;
         rhs =
         rhs = [ NT_NTtopdecllist ];
       { lhs = NT_NTtopdecllist;
         rhs = [ NT_NTtopdecl; T_Semicolon; NT_NTtopdecllist ];
         rhs = [ NT_NTtopdecl ];
       { lhs = NT_NTtopdecl;
         rhs = [ NT_NTtype; NT_NTsimpletype; T_REquals; NT_NTtype ];
         rhs = [ NT_NTdata; NT_NTsimpletype; T_REquals; NT_NTconstrs; NT_NTderiving ];
         rhs = [ NT_NTdata; NT_NTsimpletype; T_REquals; NT_NTconstrs ];
         rhs = [ NT_NTnewtype; NT_NTsimpletype; T_REquals; NT_NTnewconstr; NT_NTderiving ];
         rhs = [ NT_NTnewtype; NT_NTsimpletype; T_REquals; NT_NTnewconstr ];
         rhs = [ NT_NTclass; NT_NTscontext; T_REqualsRArrow; NT_NTtycls; NT_NTtyvar; T_RWhere; NT_NTcdecls ];
         rhs = [ NT_NTclass; NT_NTscontext; T_REqualsRArrow; NT_NTtycls; NT_NTtyvar ];
         rhs = [ NT_NTclass; NT_NTtycls; NT_NTtyvar; T_RWhere; NT_NTcdecls ];
         rhs = [ NT_NTclass; NT_NTtycls; NT_NTtyvar ];
         rhs = [ NT_NTinstance; NT_NTscontext; T_REqualsRArrow; NT_NTqtycls; NT_NTinst; T_RWhere; NT_NTidecls ];
         rhs = [ NT_NTinstance; NT_NTscontext; T_REqualsRArrow; NT_NTqtycls; NT_NTinst ];
         rhs = [ NT_NTinstance; NT_NTqtycls; NT_NTinst; T_RWhere; NT_NTidecls ];
         rhs = [ NT_NTinstance; NT_NTqtycls; NT_NTinst ];
         rhs = [ NT_NTdefault; T_LParen; T_RParen ];
         rhs = [ NT_NTdefault; T_LParen; NT_NTtypelist; T_RParen ];
         rhs = [ NT_NTdecl ];
       { lhs = NT_NTdecls;
         rhs = [ T_LCurly; T_RCurly ];
         rhs = [ T_LCurly; NT_NTdecllist; T_RCurly ];
       { lhs = NT_NTdecllist;
         rhs = [ NT_NTdecl; T_Semicolon; NT_NTdecllist ];
         rhs = [ NT_NTdecl ];
       { lhs = NT_NTdecl;
         rhs = [ NT_NTgendecl ];
         rhs = [ NT_NTfunlhs; NT_NTrhs ];
         rhs = [ NT_NTinfixpat; NT_NTrhs ];
       { lhs = NT_NTcdecls;
         rhs = [ T_LCurly; T_RCurly ];
         rhs = [ T_LCurly; NT_NTcdecllist; T_RCurly ];
       { lhs = NT_NTcdecllist;
         rhs = [ NT_NTcdecl; T_Semicolon; NT_NTcdecllist ];
         rhs = [ NT_NTcdecl ];
       { lhs = NT_NTcdecl;
         rhs = [ NT_NTgendecl ];
         rhs = [ NT_NTfunlhs; NT_NTrhs ];
         rhs = [ NT_NTvar; NT_NTrhs ];
       { lhs = NT_NTidecls;
         rhs = [ T_LCurly; T_RCurly ];
         rhs = [ T_LCurly; NT_NTidecllist; T_RCurly ];
       { lhs = NT_NTidecllist;
         rhs = [ NT_NTidecl; T_Semicolon; NT_NTidecllist ];
         rhs = [ NT_NTidecl ];
       { lhs = NT_NTidecl;
         rhs =
         rhs = [ NT_NTfunlhs; NT_NTrhs ];
         rhs = [ NT_NTvar; NT_NTrhs ];
       { lhs = NT_NTgendecl;
         rhs =
         rhs = [ NT_NTvars; T_RColonColon; NT_NTcontext; T_REqualsRArrow; NT_NTtype ];
         rhs = [ NT_NTvars; T_RColonColon; NT_NTtype ];
         rhs = [ NT_NTfixity; T_IntLit; NT_NTops ];
         rhs = [ NT_NTfixity; NT_NTops ];
       { lhs = NT_NTops;
         rhs = [ NT_NToplist ];
       { lhs = NT_NToplist;
         rhs = [ NT_NTop; T_Comma; NT_NToplist ];
         rhs = [ NT_NTop ];
       { lhs = NT_NTvars;
         rhs = [ NT_NTvarlist ];
       { lhs = NT_NTvarlist;
         rhs = [ NT_NTvar; T_Comma; NT_NTvarlist ];
         rhs = [ NT_NTvar ];
       { lhs = NT_NTfixity;
         rhs = [ T_RInfixl ];
         rhs = [ T_RInfixr ];
         rhs = [ T_RInfix ];
       { lhs = NT_NTtype;
         rhs = [ NT_NTbtype; RDashRArrow; NT_NTatype ];
         rhs = [ NT_NTbtype ];
       { lhs = NT_NTbtype;
         rhs = [ NT_NTbtype; NT_NTatype ];
         rhs = [ NT_NTatype ];
       { lhs = NT_NTatype;
         rhs = [ NT_NTgtycon ];
         rhs = [ NT_NTtyvar ];
         rhs = [ T_LParen; NT_NTtype; T_Comma; NT_NTtypelist; T_RParen ];
         rhs = [ T_LSquare; NT_NTtype; T_RSquare ];
         rhs = [ T_LParen; NT_NTtype; T_RParen ];
       { lhs = NT_NTgtycon;
         rhs = [ NT_NTqtycon ];
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LSquare; T_RSquare ];
         rhs = [ T_LParen; RDashRArrow; T_RParen ];
         rhs = [ T_LParen; NT_NTcommalist; T_RParen ];
       { lhs = NT_NTcommalist;
         rhs = [ T_Comma; NT_NTcommalist ];
         rhs = [ T_Comma ];
       { lhs = NT_NTcontext;
         rhs = [ NT_NTclass ];
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LParen; NT_NTclasslist; T_RParen ];
       { lhs = NT_NTclasslist;
         rhs = [ NT_NTclass; T_Comma; NT_NTclasslist ];
         rhs = [ NT_NTclass ];
       { lhs = NT_NTclass;
         rhs = [ NT_NTqtycls; NT_NTtyvar ];
         rhs = [ NT_NTqtycls; T_LParen; NT_NTtyvar; NT_NTatypelist; T_RParen ];
       { lhs = NT_NTatypelist;
         rhs = [ NT_NTatype; NT_NTatypelist ];
         rhs = [ NT_NTatype ];
       { lhs = NT_NTscontext;
         rhs = [ NT_NTsimpleclass ];
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LParen; NT_NTsimpleclasslist; T_RParen ];
       { lhs = NT_NTsimpleclasslist;
         rhs = [ NT_NTsimpleclass; T_Comma; NT_NTsimpleclasslist ];
         rhs = [ NT_NTsimpleclass ];
       { lhs = NT_NTsimpleclass;
         rhs = [ NT_NTqtycls; NT_NTtyvar ];
       { lhs = NT_NTsimpletype;
         rhs = [ NT_NTtycon ];
         rhs = [ NT_NTtycon; NT_NTtyvarlist ];
       { lhs = NT_NTtyvarlist;
         rhs = [ NT_NTtyvar; NT_NTtyvarlist ];
         rhs = [ NT_NTtyvar ];
       { lhs = NT_NTconstrs;
         rhs = [ NT_NTconstrlist ];
       { lhs = NT_NTconstrlist;
         rhs = [ NT_NTconstr; T_RPipe; NT_NTconstrlist ];
         rhs = [ NT_NTconstr ];
       { lhs = NT_NTconstr;
         rhs = [ NT_NTcon ];
         rhs = [ NT_NTcon; NT_NTbangatypelist ];
         rhs = [ NT_NTbtype; NT_NTconop; NT_NTbtype ];
         rhs = [ NT_NTcon; T_LCurly; T_RCurly ];
         rhs = [ NT_NTcon; T_LCurly; NT_NTfielddecllist; T_RCurly ];
       { lhs = NT_NTfielddecllist;
         rhs = [ NT_NTfielddecl; T_Comma; NT_NTfielddecllist ];
         rhs = [ NT_NTfielddecl ];
       { lhs = NT_NTnewconstr;
         rhs = [ NT_NTcon; NT_NTatype ];
         rhs = [ NT_NTcon; T_LCurly; NT_NTvar; T_RColonColon; NT_NTtype; T_RCurly ];
       { lhs = NT_NTfielddecl;
         rhs = [ NT_NTvars; T_RColonColon; NT_NTtype ];
       { lhs = NT_NTderiving;
         rhs = [ T_RDeriving; NT_NTdclass ];
         rhs = [ T_RDeriving; T_LParen; T_RParen ];
         rhs = [ T_RDeriving; T_LParen; NT_NTdclasslist; T_RParen ];
       { lhs = NT_NTdclasslist;
         rhs = [ NT_NTdclass; T_Comma; NT_NTdclasslist ];
         rhs = [ NT_NTdclass ];
       { lhs = NT_NTdclass;
         rhs = [ NT_NTqtycls ];
       { lhs = NT_NTinst;
         rhs = [ NT_NTgtycon ];
         rhs = [ T_LParen; NT_NTgtycon; T_RParen ];
         rhs = [ T_LParen; NT_NTgtycon; NT_NTtyvarlist; T_RParen ];
         rhs = [ T_LParen; NT_NTtyvar; T_Comma; NT_NTtyvarcommalist; T_RParen ];
         rhs = [ T_LSquare; NT_NTtyvar; T_RSquare ];
         rhs = [ T_LParen; NT_NTtyvar; RDashRArrow; NT_NTtyvar; T_RParen ];
       { lhs = NT_NTtyvarcommalist;
         rhs = [ NT_NTtyvar; T_Comma; NT_NTtyvarcommalist ];
         rhs = [ NT_NTtyvar ];
       { lhs = NT_NTfunlhs;
         rhs = [ NT_NTvar; NT_NTapatlist ];
         rhs = [ NT_NTinfixpat ];
         rhs = [ T_LParen; NT_NTfunlhs; T_RParen; NT_NTapatlist ];
       { lhs = NT_NTapatlist;
         rhs = [ NT_NTapat; NT_NTapatlist ];
         rhs = [ NT_NTapat ];
       { lhs = NT_NTrhs;
         rhs = [ T_REquals; NT_NTexp; T_RWhere; NT_NTdecls ];
         rhs = [ T_REquals; NT_NTexp ];
         rhs = [ NT_NTgdrhs; T_RWhere; NT_NTdecls ];
         rhs = [ NT_NTgdrhs ];
       { lhs = NT_NTgdrhs;
         rhs = [ NT_NTgd; T_REquals; NT_NTexp; NT_NTgdrhs ];
         rhs = [ NT_NTgd; T_REquals; NT_NTexp ];
       { lhs = NT_NTgd;
         rhs = [ NT_NTinfixexp ];
       { lhs = NT_NTexp;
         rhs = [ NT_NTinfixexp; T_RColonColon; NT_NTcontext; T_REqualsRArrow; NT_NTtype ];
         rhs = [ NT_NTinfixexp; T_RColonColon; NT_NTtype ];
         rhs = [ NT_NTinfixexp ];
       { lhs = NT_NTinfixexp;
         rhs = [ NT_NTexp10; NT_NTqop; NT_NTinfixexp ];
         rhs = [ NT_NTexp10 ];
       { lhs = NT_NTexp10;
         rhs = [ T_RBackslash; NT_NTapatlist; RDashRArrow; NT_NTexp ];
         rhs = [ T_RLet; NT_NTdecls; T_RIn; NT_NTexp ];
         rhs = [ T_RIf; NT_NTexp; T_RThen; NT_NTexp; T_RElse; NT_NTexp ];
         rhs = [ T_RCase; NT_NTexp; T_ROf; T_LCurly; NT_NTalts; T_RCurly ];
         rhs = [ T_RDo; T_LCurly; NT_NTstmts; T_RCurly ];
         rhs = [ NT_NTfexp ];
       { lhs = NT_NTfexp;
         rhs = [ NT_NTfexp; NT_NTaexp ];
         rhs = [ NT_NTaexp ];
       { lhs = NT_NTaexp;
         rhs = [ NT_NTqvar ];
         rhs = [ NT_NTgcon ];
         rhs = [ NT_NTliteral ];
         rhs = [ T_LParen; NT_NTexp; T_RParen ];
         rhs = [ T_LParen; NT_NTexp; T_Comma; NT_NTexplist; T_RParen ];
         rhs = [ T_LSquare; NT_NTexplist; T_RSquare ];
         rhs = [ T_LSquare; NT_NTexp; T_Comma; NT_NTexp; T_RDotDot; NT_NTexp; T_RSquare ];
         rhs = [ T_LSquare; NT_NTexp; T_Comma; NT_NTexp; T_RDotDot; T_RSquare ];
         rhs = [ T_LSquare; NT_NTexp; T_RDotDot; NT_NTexp; T_RSquare ];
         rhs = [ T_LSquare; NT_NTexp; T_RDotDot; T_RSquare ];
         rhs = [ T_LSquare; NT_NTexp; T_RPipe; NT_NTquallist; T_RSquare ];
         rhs = [ T_LParen; NT_NTinfixexp; NT_NTqop; T_RParen ];
         rhs = [ T_LParen; NT_NTqop; NT_NTinfixexp; T_RParen ];
         rhs = [ NT_NTqcon; T_LCurly; T_RCurly ];
         rhs = [ NT_NTqcon; T_LCurly; NT_NTfbindlist; T_RCurly ];
         rhs = [ NT_NTaexp; T_LCurly; NT_NTfbindlist; T_RCurly ];
       { lhs = NT_NTexplist;
         rhs = [ NT_NTexp; T_Comma; NT_NTexplist ];
         rhs = [ NT_NTexp ];
       { lhs = NT_NTquallist;
         rhs = [ NT_NTqual; T_Comma; NT_NTquallist ];
         rhs = [ NT_NTqual ];
       { lhs = NT_NTfbindlist;
         rhs = [ NT_NTfbind; T_Comma; NT_NTfbindlist ];
         rhs = [ NT_NTfbind ];
       { lhs = NT_NTqual;
         rhs = [ NT_NTpat; T_RLArrowDash; NT_NTexp ];
         rhs = [ T_RLet; NT_NTdecls ];
         rhs = [ NT_NTexp ];
       { lhs = NT_NTalts;
         rhs = [ NT_NTaltlist ];
       { lhs = NT_NTaltlist;
         rhs = [ NT_NTalt; T_Semicolon; NT_NTaltlist ];
         rhs = [ NT_NTalt ];
       { lhs = NT_NTalt;
         rhs = [ NT_NTpat; RDashRArrow; NT_NTexp; T_RWhere; NT_NTdecls ];
         rhs = [ NT_NTpat; RDashRArrow; NT_NTexp ];
         rhs = [ NT_NTpat; NT_NTgdpat; T_RWhere; NT_NTdecls ];
         rhs = [ NT_NTpat; NT_NTgdpat ];
       { lhs = NT_NTgdpat;
         rhs = [ NT_NTgd; RDashRArrow; NT_NTexp; NT_NTgdpat ];
         rhs = [ NT_NTgd; RDashRArrow; NT_NTexp ];
       { lhs = NT_NTstmts;
         rhs = [ NT_NTstmtlist; NT_NTexp; T_Semicolon ];
         rhs = [ NT_NTstmtlist; NT_NTexp ];
         rhs = [ NT_NTexp; T_Semicolon ];
         rhs = [ NT_NTexp ];
       { lhs = NT_NTstmt;
         rhs = [ NT_NTexp; T_Semicolon ];
         rhs = [ NT_NTpat; T_RLArrowDash; NT_NTexp; T_Semicolon ];
         rhs = [ NT_NTlet; NT_NTdecls; T_Semicolon ];
         rhs = [ T_Semicolon ];
       { lhs = NT_NTfbind;
         rhs = [ NT_NTqvar; T_REquals; NT_NTexp ];
       { lhs = NT_NTpat;
         rhs = [ NT_NTinfixpat ];
       { lhs = NT_NTinfixpat;
         rhs = [ NT_NTpat10; NT_NTqconop; NT_NTinfixpat ];
         rhs = [ NT_NTpat10 ];
       { lhs = NT_NTpat10;
         rhs = [ NT_NTapat ];
         rhs = [ NT_NTgcon; NT_NTapatlist ];
       { lhs = NT_NTapatlist;
         rhs = [ NT_NTapat; NT_NTapatlist ];
         rhs = [ NT_NTapat ];
       { lhs = NT_NTapat;
         rhs = [ NT_NTvar; T_RAt; NT_NTapat ];
         rhs = [ NT_NTvar ];
         rhs = [ NT_NTgcon ];
         rhs = [ NT_NTqcon; T_LCurly; T_RCurly ];
         rhs = [ NT_NTqcon; T_LCurly; NT_NTfpatlist; T_RCurly ];
         rhs = [ NT_NTliteral ];
         rhs = [ T_RUnderscore ];
         rhs = [ T_LParen; NT_NTpat; T_RParen ];
         rhs = [ T_LParen; NT_NTpat; NT_NTpatlist; T_RParen ];
         rhs = [ T_LSquare; NT_NTpatlist; T_RSquare ];
         rhs = [ T_RTilde; NT_NTapat ];
       { lhs = NT_NTfpatlist;
         rhs = [ NT_NTfpat; T_Comma; NT_NTfpatlist ];
         rhs = [ NT_NTfpat ];
       { lhs = NT_NTpatlist;
         rhs = [ NT_NTpat; T_Comma; NT_NTpatlist ];
         rhs = [ NT_NTpat ];
       { lhs = NT_NTfpat;
         rhs = [ NT_NTqvar; T_REquals; NT_NTpat ];
       { lhs = NT_NTgcon;
         rhs = [ T_LParen; T_RParen ];
         rhs = [ T_LSquare; T_RSquare ];
         rhs = [ T_LParen; NT_NTcommalist; T_RParen ];
         rhs = [ NT_NTqcon ];
       { lhs = NT_NTvar;
         rhs = [ T_VarId ];
         rhs = [ T_LParen; T_VarSym; T_RParen ];
       { lhs = NT_NTqvar;
         rhs = [ T_QVarId ];
         rhs = [ T_VarId ];
         rhs = [ T_LParen; T_QVarSym; T_RParen ];
         rhs = [ T_LParen; T_VarSym; T_RParen ];
       { lhs = NT_NTcon;
         rhs = [ T_ConId ];
         rhs = [ T_LParen; T_ConSym; T_RParen ];
       { lhs = NT_NTqcon;
         rhs = [ T_QConId ];
         rhs = [ T_ConId ];
         rhs = [ T_LParen; T_RColon; T_RParen ];
         rhs = [ T_LParen; T_QConSym; T_RParen ];
         rhs = [ T_LParen; T_ConSym; T_RParen ];
       { lhs = NT_NTvarop;
         rhs = [ T_VarSym ];
         rhs = [ T_Backquote; T_VarId; T_Backquote ];
       { lhs = NT_NTqvarop;
         rhs = [ T_QVarSym ];
         rhs = [ T_VarSym ];
         rhs = [ T_Backquote; T_QVarId; T_Backquote ];
         rhs = [ T_Backquote; T_VarId; T_Backquote ];
       { lhs = NT_NTconop;
         rhs = [ T_ConSym ];
         rhs = [ T_Backquote; T_ConId; T_Backquote ];
       { lhs = NT_NTqconop;
         rhs = [ T_QConSym ];
         rhs = [ T_ConSym ];
         rhs = [ T_Backquote; T_QConId; T_Backquote ];
         rhs = [ T_Backquote; T_ConId; T_Backquote ];
       { lhs = NT_NTop;
         rhs = [ NT_NTvarop ];
         rhs = [ NT_NTconop ];
       { lhs = NT_NTqop;
         rhs = [ NT_NTqvarop ];
         rhs = [ NT_NTqconop ];
       { lhs = NT_NTgconsym;
         rhs = [ T_RColon ];
         rhs = [ T_QConSym ];
       { lhs = NT_NTtyvar;
         rhs = [ T_VarId ];
       { lhs = NT_NTtycon;
         rhs = [ T_ConId ];
       { lhs = NT_NTtycls;
         rhs = [ T_ConId ];
       { lhs = NT_NTmodid;
         rhs = [ T_ConId ];
       { lhs = NT_NTqtycon;
         rhs = [ T_QConId ];
         rhs = [ T_ConId ];
       { lhs = NT_NTqtycls;
         rhs = [ T_QConId ];
         rhs = [ T_ConId ];
       { lhs = NT_NTliteral;
         rhs = [ T_IntLit ];
         rhs = [ T_FloatLit ];
         rhs = [ T_CharLit ];
         rhs = [ T_StringLit ];
    |];
  terminal_action = (fun _ -> 0);
}
*)
