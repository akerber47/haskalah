open Batteries
;;

(* Comment above each AST node says what sort of children it *should* have. No
 * type system guarantees. *)
type ast0 = {
  node : ast0node;
  (* For error reporting purposes, start and end of this syntax block in
   * source. Basically just min/max over all tokens that make up the block.
   * (ignoring implicitly generated tokens) *)
  blockstart : int;
  blockend   : int;
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
  (* NOTE no datatype contexts in our grammar *)
  (* simpletype constr* [deriving] *)
  | `Topdecl_data of ast0 * ast0 list * ast0 option
  (* simpletype newconstr [deriving] *)
  | `Topdecl_newtype of ast0 * ast0 * ast0 option
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
  | `Funlhs_app of ast0 * ast0 list
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
  | Goal
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
  | NTinfixexp (* see note above *)
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
  | NTinfixpat (* see note above *)
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
  | NTgconsym
  (* Above are all the non-terminals listed in the printed grammar. To actually
   * implement this grammar without wildcards in our BNF syntax, we need a few
   * more. Each bunch below listed in order of appearance on the right-hand
   * sides of the printed grammar in the Report. Luckily each sort of
   * expression generally only has one kind of separator (not different
   * separators depending on where it shows up).
   * The idea here is instead of (say) the production
   *   apat -> qcon { fpat_1 , ... , fpat_k } (k >= 1)
   * we need to use the productions
   *   apat -> qcon { apatlist }
   *   apatlist -> fpat , apatlist
   *             | fpat
   * so we'll just implement it like that. Basically we're expanding all the
   * wildcards, optional things, etc in the BNF grammar by hand. Each
   * <stuff>list represents a list of AT LEAST ONE <stuff>. *)
  (* Comma-separated *)
  | NTimpdecllist
  | NTexportlist
  | NTcnamelist
  | NTqvarlist
  | NTimportlist
  | NTtopdecllist
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
  | NTtyvarcommalist
  (* Semicolon-separated *)
  | NTdecllist
  | NTcdecllist
  | NTidecllist
  | NTaltlist
  (* Pipe separated *)
  | NTconstrlist
  (* Whitespace (nothing) separated *)
  | NTatypelist
  | NTtyvarlist
  | NTapatlist
  | NTstmtlist
  (* Stuff that "really" belongs in the lexical grammar, but we'll deal with it
   * here bc otherwise token types become ambiguous (and lexer gets
   * overcomplicated) *)
  | NTtyvar
  | NTtycon
  | NTtycls
  | NTmodid
  | NTqtycon
  | NTqtycls
  | NTliteral

let nonterm_print o nt =
  let s =
    match nt with
    | Goal -> "Goal"
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
    | NTgconsym -> "gconsym"
    | NTimpdecllist -> "impdecllist"
    | NTexportlist -> "exportlist"
    | NTcnamelist -> "cnamelist"
    | NTqvarlist -> "qvarlist"
    | NTimportlist -> "importlist"
    | NTtopdecllist -> "topdecllist"
    | NTvarlist -> "varlist"
    | NTtypelist -> "typelist"
    | NTcommalist -> "commalist"
    | NToplist -> "oplist"
    | NTclasslist -> "classlist"
    | NTsimpleclasslist -> "simpleclasslist"
    | NTconstrlist -> "constrlist"
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
    | NTtyvarcommalist -> "tyvarcommalist"
    | NTapatlist -> "apatlist"
    | NTaltlist -> "altlist"
    | NTstmtlist -> "stmtlist"
    | NTtyvar -> "NTtyvar"
    | NTtycon -> "NTtycon"
    | NTtycls -> "NTtycls"
    | NTmodid -> "NTmodid"
    | NTqtycon -> "NTqtycon"
    | NTqtycls -> "NTqtycls"
    | NTliteral -> "NTliteral"
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


open Lex

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
         rhs = [ NT NTmodule ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; T RWhere; NT NTbody ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; NT NTexports; T RWhere; NT NTbody ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodule;
         rhs = [ NT NTbody ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [ T LCurly; NT NTimpdecls; T Semicolon; NT NTtopdecls; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [ T LCurly; NT NTimpdecls; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [ T LCurly; NT NTtopdecls; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbody;
         rhs = [ T LCurly; NT NTtopdecls; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecls;
         rhs = [ NT NTimpdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecllist;
         rhs = [ NT NTimpdecl; T Semicolon; NT NTimpdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecllist;
         rhs = [ NT NTimpdecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexports;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexports;
         rhs = [ T LParen; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexportlist;
         rhs = [ NT NTexport; T Comma; NT NTexportlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexportlist;
         rhs = [ NT NTexport ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycon; T LParen; T RDotDot; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycon; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycon; T LParen; NT NTcnamelist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycls; T LParen; T RDotDot; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycls; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ NT NTqtycls; T LParen; NT NTqvarlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexport;
         rhs = [ T RModule; NT NTmodid ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; T VarId; NT NTmodid; T VarId; NT NTmodid; NT NTimpspec ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; T VarId; NT NTmodid; T VarId; NT NTmodid ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; T VarId; NT NTmodid; NT NTimpspec ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; T VarId; NT NTmodid ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; NT NTmodid; T VarId; NT NTmodid; NT NTimpspec ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; NT NTmodid; T VarId; NT NTmodid ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; NT NTmodid; NT NTimpspec ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpdecl;
         rhs = [ T RImport; NT NTmodid ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T LParen; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T Comma; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimportlist;
         rhs = [ NT NTimport; T Comma; NT NTimportlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimportlist;
         rhs = [ NT NTimport ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycon; T LParen; T RDotDot; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycon; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycon; T LParen; NT NTcnamelist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycls; T LParen; T RDotDot; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycls; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTimport;
         rhs = [ NT NTtycls; T LParen; NT NTvarlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcname;
         rhs = [ NT NTvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcname;
         rhs = [ NT NTcon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecls;
         rhs = [];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecls;
         rhs = [ NT NTtopdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl; T Semicolon; NT NTtopdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ NT NTtype; NT NTsimpletype; T REquals; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs; NT NTderiving ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr; NT NTderiving ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; NT NTtycls; NT NTtyvar; T RWhere; NT NTcdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; NT NTtycls; NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTtycls; NT NTtyvar; T RWhere; NT NTcdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTtycls; NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqtycls; NT NTinst; T RWhere; NT NTidecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqtycls; NT NTinst ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqtycls; NT NTinst; T RWhere; NT NTidecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqtycls; NT NTinst ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; NT NTtypelist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtopdecl;
         rhs = [ NT NTdecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecls;
         rhs = [ T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecls;
         rhs = [ T LCurly; NT NTdecllist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecllist;
         rhs = [ NT NTdecl; T Semicolon; NT NTdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecllist;
         rhs = [ NT NTdecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecl;
         rhs = [ NT NTgendecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecl;
         rhs = [ NT NTfunlhs; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdecl;
         rhs = [ NT NTinfixpat; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecls;
         rhs = [ T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecls;
         rhs = [ T LCurly; NT NTcdecllist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecllist;
         rhs = [ NT NTcdecl; T Semicolon; NT NTcdecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecllist;
         rhs = [ NT NTcdecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecl;
         rhs = [ NT NTgendecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecl;
         rhs = [ NT NTfunlhs; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcdecl;
         rhs = [ NT NTvar; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecls;
         rhs = [ T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecls;
         rhs = [ T LCurly; NT NTidecllist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecllist;
         rhs = [ NT NTidecl; T Semicolon; NT NTidecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecllist;
         rhs = [ NT NTidecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecl;
         rhs = [];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecl;
         rhs = [ NT NTfunlhs; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTidecl;
         rhs = [ NT NTvar; NT NTrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgendecl;
         rhs = [];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgendecl;
         rhs = [ NT NTvars; T RColonColon; NT NTcontext; T REqualsRArrow; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgendecl;
         rhs = [ NT NTvars; T RColonColon; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgendecl;
         rhs = [ NT NTfixity; T IntLit; NT NTops ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgendecl;
         rhs = [ NT NTfixity; NT NTops ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTops;
         rhs = [ NT NToplist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NToplist;
         rhs = [ NT NTop; T Comma; NT NToplist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NToplist;
         rhs = [ NT NTop ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvars;
         rhs = [ NT NTvarlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvarlist;
         rhs = [ NT NTvar; T Comma; NT NTvarlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvarlist;
         rhs = [ NT NTvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfixity;
         rhs = [ T RInfixl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfixity;
         rhs = [ T RInfixr ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfixity;
         rhs = [ T RInfix ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtype;
         rhs = [ NT NTbtype; T RDashRArrow; NT NTatype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtype;
         rhs = [ NT NTbtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbtype;
         rhs = [ NT NTbtype; NT NTatype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTbtype;
         rhs = [ NT NTatype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatype;
         rhs = [ NT NTgtycon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatype;
         rhs = [ NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T Comma; NT NTtypelist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatype;
         rhs = [ T LSquare; NT NTtype; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgtycon;
         rhs = [ NT NTqtycon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgtycon;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgtycon;
         rhs = [ T LSquare; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgtycon;
         rhs = [ T LParen; T RDashRArrow; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgtycon;
         rhs = [ T LParen; NT NTcommalist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcommalist;
         rhs = [ T Comma; NT NTcommalist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcommalist;
         rhs = [ T Comma ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcontext;
         rhs = [ NT NTclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcontext;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcontext;
         rhs = [ T LParen; NT NTclasslist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTclasslist;
         rhs = [ NT NTclass; T Comma; NT NTclasslist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTclasslist;
         rhs = [ NT NTclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTclass;
         rhs = [ NT NTqtycls; NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTclass;
         rhs = [ NT NTqtycls; T LParen; NT NTtyvar; NT NTatypelist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatypelist;
         rhs = [ NT NTatype; NT NTatypelist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTatypelist;
         rhs = [ NT NTatype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTscontext;
         rhs = [ NT NTsimpleclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTscontext;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTscontext;
         rhs = [ T LParen; NT NTsimpleclasslist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass; T Comma; NT NTsimpleclasslist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTsimpleclass;
         rhs = [ NT NTqtycls; NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTsimpletype;
         rhs = [ NT NTtycon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTsimpletype;
         rhs = [ NT NTtycon; NT NTtyvarlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtyvarlist;
         rhs = [ NT NTtyvar; NT NTtyvarlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtyvarlist;
         rhs = [ NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstrs;
         rhs = [ NT NTconstrlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstrlist;
         rhs = [ NT NTconstr; T RPipe; NT NTconstrlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstrlist;
         rhs = [ NT NTconstr ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstr;
         rhs = [ NT NTcon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstr;
         rhs = [ NT NTbtype; NT NTconop; NT NTbtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; NT NTfielddecllist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl; T Comma; NT NTfielddecllist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTnewconstr;
         rhs = [ NT NTcon; NT NTatype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTnewconstr;
         rhs = [ NT NTcon; T LCurly; NT NTvar; T RColonColon; NT NTtype; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfielddecl;
         rhs = [ NT NTvars; T RColonColon; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTderiving;
         rhs = [ T RDeriving; NT NTdclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; NT NTdclasslist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdclasslist;
         rhs = [ NT NTdclass; T Comma; NT NTdclasslist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdclasslist;
         rhs = [ NT NTdclass ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTdclass;
         rhs = [ NT NTqtycls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ NT NTgtycon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; NT NTtyvarlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ T LParen; NT NTtyvar; T Comma; NT NTtyvarcommalist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ T LSquare; NT NTtyvar; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinst;
         rhs = [ T LParen; NT NTtyvar; T RDashRArrow; NT NTtyvar; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtyvarcommalist;
         rhs = [ NT NTtyvar; T Comma; NT NTtyvarcommalist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtyvarcommalist;
         rhs = [ NT NTtyvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfunlhs;
         rhs = [ NT NTvar; NT NTapatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfunlhs;
         rhs = [ NT NTinfixpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfunlhs;
         rhs = [ T LParen; NT NTfunlhs; T RParen; NT NTapatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapatlist;
         rhs = [ NT NTapat; NT NTapatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapatlist;
         rhs = [ NT NTapat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp; T RWhere; NT NTdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTrhs;
         rhs = [ NT NTgdrhs; T RWhere; NT NTdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTrhs;
         rhs = [ NT NTgdrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp; NT NTgdrhs ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgd;
         rhs = [ NT NTinfixexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp;
         rhs = [ NT NTinfixexp; T RColonColon; NT NTcontext; T REqualsRArrow; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp;
         rhs = [ NT NTinfixexp; T RColonColon; NT NTtype ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp;
         rhs = [ NT NTinfixexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinfixexp;
         rhs = [ NT NTexp10; NT NTqop; NT NTinfixexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinfixexp;
         rhs = [ NT NTexp10 ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ T RBackslash; NT NTapatlist; T RDashRArrow; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ T RLet; NT NTdecls; T RIn; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ T RIf; NT NTexp; T RThen; NT NTexp; T RElse; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ T RCase; NT NTexp; T ROf; T LCurly; NT NTalts; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ T RDo; T LCurly; NT NTstmts; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexp10;
         rhs = [ NT NTfexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfexp;
         rhs = [ NT NTfexp; NT NTaexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfexp;
         rhs = [ NT NTaexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTqvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTgcon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTliteral ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T Comma; NT NTexplist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexplist; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; NT NTexp; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; NT NTexp; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RPipe; NT NTquallist; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTinfixexp; NT NTqop; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ T LParen; NT NTqop; NT NTinfixexp; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTqcon; T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTqcon; T LCurly; NT NTfbindlist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaexp;
         rhs = [ NT NTaexp; T LCurly; NT NTfbindlist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexplist;
         rhs = [ NT NTexp; T Comma; NT NTexplist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTexplist;
         rhs = [ NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTquallist;
         rhs = [ NT NTqual; T Comma; NT NTquallist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTquallist;
         rhs = [ NT NTqual ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfbindlist;
         rhs = [ NT NTfbind; T Comma; NT NTfbindlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfbindlist;
         rhs = [ NT NTfbind ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqual;
         rhs = [ NT NTpat; T RLArrowDash; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqual;
         rhs = [ T RLet; NT NTdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqual;
         rhs = [ NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTalts;
         rhs = [ NT NTaltlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaltlist;
         rhs = [ NT NTalt; T Semicolon; NT NTaltlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTaltlist;
         rhs = [ NT NTalt ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTalt;
         rhs = [ NT NTpat; T RDashRArrow; NT NTexp; T RWhere; NT NTdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTalt;
         rhs = [ NT NTpat; T RDashRArrow; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTalt;
         rhs = [ NT NTpat; NT NTgdpat; T RWhere; NT NTdecls ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTalt;
         rhs = [ NT NTpat; NT NTgdpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp; NT NTgdpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmts;
         rhs = [ NT NTstmtlist; NT NTexp; T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmts;
         rhs = [ NT NTstmtlist; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmts;
         rhs = [ NT NTexp; T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmts;
         rhs = [ NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmt;
         rhs = [ NT NTexp; T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmt;
         rhs = [ NT NTpat; T RLArrowDash; NT NTexp; T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmt;
         rhs = [ T RLet; NT NTdecls; T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmt;
         rhs = [ T Semicolon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfbind;
         rhs = [ NT NTqvar; T REquals; NT NTexp ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTpat;
         rhs = [ NT NTinfixpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinfixpat;
         rhs = [ NT NTpat10; NT NTqconop; NT NTinfixpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTinfixpat;
         rhs = [ NT NTpat10 ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTpat10;
         rhs = [ NT NTapat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTpat10;
         rhs = [ NT NTgcon; NT NTapatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapatlist;
         rhs = [ NT NTapat; NT NTapatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapatlist;
         rhs = [ NT NTapat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTvar; T RAt; NT NTapat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTvar ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTgcon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTqcon; T LCurly; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTqcon; T LCurly; NT NTfpatlist; T RCurly ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ NT NTliteral ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ T RUnderscore ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ T LParen; NT NTpat; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ T LParen; NT NTpat; NT NTpatlist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ T LSquare; NT NTpatlist; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTapat;
         rhs = [ T RTilde; NT NTapat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfpatlist;
         rhs = [ NT NTfpat; T Comma; NT NTfpatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfpatlist;
         rhs = [ NT NTfpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTpatlist;
         rhs = [ NT NTpat; T Comma; NT NTpatlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTpatlist;
         rhs = [ NT NTpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTfpat;
         rhs = [ NT NTqvar; T REquals; NT NTpat ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgcon;
         rhs = [ T LParen; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgcon;
         rhs = [ T LSquare; T RSquare ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgcon;
         rhs = [ T LParen; NT NTcommalist; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgcon;
         rhs = [ NT NTqcon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvar;
         rhs = [ T VarId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvar;
         rhs = [ T LParen; T VarSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvar;
         rhs = [ T QVarId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvar;
         rhs = [ T VarId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvar;
         rhs = [ T LParen; T QVarSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvar;
         rhs = [ T LParen; T VarSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcon;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTcon;
         rhs = [ T LParen; T ConSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqcon;
         rhs = [ T QConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqcon;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqcon;
         rhs = [ T LParen; T RColon; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqcon;
         rhs = [ T LParen; T QConSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqcon;
         rhs = [ T LParen; T ConSym; T RParen ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvarop;
         rhs = [ T VarSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTvarop;
         rhs = [ T Backquote; T VarId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvarop;
         rhs = [ T QVarSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvarop;
         rhs = [ T VarSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvarop;
         rhs = [ T Backquote; T QVarId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqvarop;
         rhs = [ T Backquote; T VarId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconop;
         rhs = [ T ConSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTconop;
         rhs = [ T Backquote; T ConId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqconop;
         rhs = [ T QConSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqconop;
         rhs = [ T ConSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqconop;
         rhs = [ T Backquote; T QConId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqconop;
         rhs = [ T Backquote; T ConId; T Backquote ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTop;
         rhs = [ NT NTvarop ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTop;
         rhs = [ NT NTconop ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqop;
         rhs = [ NT NTqvarop ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqop;
         rhs = [ NT NTqconop ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgconsym;
         rhs = [ T RColon ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTgconsym;
         rhs = [ T QConSym ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtyvar;
         rhs = [ T VarId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtycon;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTtycls;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTmodid;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqtycon;
         rhs = [ T QConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqtycon;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqtycls;
         rhs = [ T QConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTqtycls;
         rhs = [ T ConId ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTliteral;
         rhs = [ T IntLit ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTliteral;
         rhs = [ T FloatLit ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTliteral;
         rhs = [ T CharLit ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTliteral;
         rhs = [ T StringLit ];
         semantic_action = (fun _ -> 0);};
    |];
  terminal_action = (fun _ -> 0);
}

let parse lxq =
  match generate haskell_cfg lxq with
  | 0 -> { node = `Gcon_tuple 1; blockstart = -1; blockend = -1 }
  | _ -> { node = `Gcon_tuple 0; blockstart = -1; blockend = -1 }
;;
