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
 * Fixity resolution is later (made in Haskell 2010)
 * https://ghc.haskell.org/trac/haskell-prime/wiki/FixityResolution
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

       { lhs = body
         rhs = { topdecls }
       { lhs = body
         rhs = { topdecls }
       { lhs = impdecls
         rhs = impdecllist
       { lhs = impdecllist
         rhs = impdecl ; impdecllist
         rhs = impdecl
       { lhs = exports
         rhs = ( )
         rhs = ( , )
         rhs = ( exportlist )
         rhs = ( exportlist , )
       { lhs = exportlist
         rhs = export , exportlist
         rhs = export
       { lhs = export
         rhs = qvar
         rhs = qtycon ( .. )
         rhs = qtycon ( )
         rhs = qtycon ( cnamelist )
         rhs = qtycls ( .. )
         rhs = qtycls ( )
         rhs = qtycls ( qvarlist )
         rhs = RModule modid
       { lhs = impdecl
         rhs = RImport RQualified modid RAs modid impspec
         rhs = RImport RQualified modid RAs modid
         rhs = RImport RQualified modid impspec
         rhs = RImport RQualified modid
         rhs = RImport modid RAs modid impspec
         rhs = RImport modid RAs modid
         rhs = RImport modid impspec
         rhs = RImport modid
       { lhs = impspec
         rhs = RHiding ( )
         rhs = RHiding ( , )
         rhs = RHiding ( importlist )
         rhs = RHiding ( importlist , )
         rhs = ( )
         rhs = ( , )
         rhs = ( importlist )
         rhs = ( importlist , )
       { lhs = importlist
         rhs = import , importlist
         rhs = import
       { lhs = import
         rhs = var
         rhs = tycon ( .. )
         rhs = tycon (  )
         rhs = tycon ( cnamelist )
         rhs = tycls ( .. )
         rhs = tycls ( )
         rhs = tycls ( varlist )
       { lhs = cname
         rhs = var
         rhs = con
       { lhs = topdecls
         rhs =
         rhs = topdecllist
       { lhs = topdecllist
         rhs = topdecl ; topdecllist
         rhs = topdecl
       { lhs = topdecl
         rhs = type simpletype = type
         rhs = data simpletype = constrs deriving
         rhs = data simpletype = constrs
         rhs = newtype simpletype = newconstr deriving
         rhs = newtype simpletype = newconstr
         rhs = class scontext => tycls tyvar RWhere cdecls
         rhs = class scontext => tycls tyvar
         rhs = class tycls tyvar RWhere cdecls
         rhs = class tycls tyvar
         rhs = instance scontext => qtycls inst RWhere idecls
         rhs = instance scontext => qtycls inst
         rhs = instance qtycls inst RWhere idecls
         rhs = instance qtycls inst
         rhs = default ( )
         rhs = default ( typelist )
         rhs = decl
       { lhs = decls
         rhs = { }
         rhs = { decllist }
       { lhs = decllist
         rhs = decl ; decllist
         rhs = decl
       { lhs = decl
         rhs = gendecl
         rhs = funlhs rhs
         rhs = infixpat rhs
       { lhs = cdecls
         rhs = { }
         rhs = { cdecllist }
       { lhs = cdecllist
         rhs = cdecl ; cdecllist
         rhs = cdecl
       { lhs = cdecl
         rhs = gendecl
         rhs = funlhs rhs
         rhs = var rhs
       { lhs = idecls
         rhs = { }
         rhs = { idecllist }
       { lhs = idecllist
         rhs = idecl ; idecllist
         rhs = idecl
       { lhs = idecl
         rhs =
         rhs = funlhs rhs
         rhs = var rhs
       { lhs = gendecl
         rhs =
         rhs = vars :: context => type
         rhs = vars :: type
         rhs = fixity IntLit ops
         rhs = fixity ops
       { lhs = ops
         rhs = oplist
       { lhs = oplist
         rhs = op , oplist
         rhs = op
       { lhs = vars
         rhs = varlist
       { lhs = varlist
         rhs = var , varlist
         rhs = var
       { lhs = fixity
         rhs = RInfixl
         rhs = RInfixr
         rhs = RInfix
       { lhs = type
         rhs = btype -> atype
         rhs = btype
       { lhs = btype
         rhs = btype atype
         rhs = atype
       { lhs = atype
         rhs = gtycon
         rhs = tyvar
         rhs = ( type , typelist )
         rhs = [ type ]
         rhs = ( type )
       { lhs = gtycon
         rhs = qtycon
         rhs = ( )
         rhs = [ ]
         rhs = ( -> )
         rhs = ( commalist )
       { lhs = commalist
         rhs = , commalist
         rhs = ,
       { lhs = context
         rhs = class
         rhs = ( )
         rhs = ( classlist )
       { lhs = classlist
         rhs = class , classlist
         rhs = class
       { lhs = class
         rhs = qtycls tyvar
         rhs = qtycls ( tyvar atypelist )
       { lhs = atypelist
         rhs = atype atypelist
         rhs = atype
       { lhs = scontext
         rhs = simpleclass
         rhs = ( )
         rhs = ( simpleclasslist )
       { lhs = simpleclasslist
         rhs = simpleclass , simpleclasslist
         rhs = simpleclass
       { lhs = simpleclass
         rhs = qtycls tyvar
       { lhs = simpletype
         rhs = tycon
         rhs = tycon tyvarlist
       { lhs = tyvarlist
         rhs = tyvar tyvarlist
         rhs = tyvar
       { lhs = constrs
         rhs = constrlist
       { lhs = constrlist
         rhs = constr | constrlist
         rhs = constr
       { lhs = constr
         rhs = con
         rhs = con bangatypelist
         rhs = ! atype conop ! atype
         rhs = btype conop ! atype
         rhs = ! atype conop ! btype
         rhs = btype conop btype
         rhs = con { }
         rhs = con { fielddecllist }
       { lhs = bangatypelist
         rhs = ! atype bangatypelist
         rhs = atype bangatypelist
         rhs = ! atype
         rhs = atype
       { lhs = fielddecllist
         rhs = fielddecl , fielddecllist
         rhs = fielddecl
       { lhs = newconstr
         rhs = con atype
         rhs = con { var :: type }
       { lhs = fielddecl
         rhs = vars :: type
         rhs = vars :: ! atype
       { lhs = deriving
         rhs = RDeriving dclass
         rhs = RDeriving ( )
         rhs = RDeriving ( dclasslist )
       { lhs = dclasslist
         rhs = dclass , dclasslist
         rhs = dclass
       { lhs = dclass
         rhs = qtycls
       { lhs = inst
         rhs = gtycon
         rhs = ( gtycon )
         rhs = ( gtycon tyvarlist )
         rhs = ( tyvar , tyvarcommalist )
         rhs = [ tyvar ]
         rhs = ( tyvar -> tyvar )
       { lhs = tyvarcommalist
         rhs = tyvar , tyvarcommalist
         rhs = tyvar
       { lhs = funlhs
         rhs = var apatlist
         rhs = infixpat
         rhs = ( funlhs ) apatlist
       { lhs = apatlist
         rhs = apat apatlist
         rhs = apat
       { lhs = rhs
         rhs = = exp RWhere decls
         rhs = = exp
         rhs = gdrhs RWhere decls
         rhs = gdrhs
       { lhs = gdrhs
         rhs = gd = exp gdrhs
         rhs = gd = exp
       { lhs = gd
         rhs = infixexp
       { lhs = exp
         rhs = infixexp :: context => type
         rhs = infixexp :: type
         rhs = infixexp
       { lhs = infixexp
         rhs = exp10 qop infixexp
         rhs = - infixexp
         rhs = exp10
       { lhs = exp10
         rhs = \ apatlist -> exp
         rhs = RLet decls RIn exp
         rhs = RIf exp RThen exp RElse exp
         rhs = RCase exp ROf { alts }
         rhs = RDo { stmts }
         rhs = fexp
       { lhs = fexp
         rhs = fexp aexp
         rhs = aexp
       { lhs = aexp
         rhs = qvar
         rhs = gcon
         rhs = literal
         rhs = ( exp )
         rhs = ( exp , explist )
         rhs = [ explist ]
         rhs = [ exp , exp .. exp ]
         rhs = [ exp , exp .. ]
         rhs = [ exp .. exp ]
         rhs = [ exp .. ]
         rhs = [ exp | quallist ]
         rhs = ( infixexp qop )
         rhs = ( qop infixexp )
         rhs = qcon { }
         rhs = qcon { fbindlist }
         rhs = aexp { fbindlist }
       { lhs = explist
         rhs = exp , explist
         rhs = exp
       { lhs = quallist
         rhs = qual , quallist
         rhs = qual
       { lhs = fbindlist
         rhs = fbind , fbindlist
         rhs = fbind
       { lhs = qual
         rhs = pat <- exp
         rhs = RLet decls
         rhs = exp
       { lhs = alts
         rhs = altlist
       { lhs = altlist
         rhs = alt ; altlist
         rhs = alt
       { lhs = alt
         rhs = pat -> exp RWhere decls
         rhs = pat -> exp
         rhs = pat gdpat RWhere decls
         rhs = pat gdpat
       { lhs = gdpat
         rhs = gd -> exp gdpat
         rhs = gd -> exp
       { lhs = stmts
         rhs = stmtlist exp ;
         rhs = stmtlist exp
         rhs = exp ;
         rhs = exp
       { lhs = stmt
         rhs = exp ;
         rhs = pat <- exp ;
         rhs = let decls ;
         rhs = ;
       { lhs = fbind
         rhs = qvar = exp
       { lhs = pat
         rhs = infixpat
       { lhs = infixpat
         rhs = pat10 qconop infixpat
         rhs = - IntLit
         rhs = - FloatLit
         rhs = pat10
       { lhs = pat10
         rhs = apat
         rhs = gcon apatlist
       { lhs = apatlist
         rhs = apat apatlist
         rhs = apat
       { lhs = apat
         rhs = var @ apat
         rhs = var
         rhs = gcon
         rhs = qcon { }
         rhs = qcon { fpatlist }
         rhs = literal
         rhs = _
         rhs = ( pat )
         rhs = ( pat patlist )
         rhs = [ patlist ]
         rhs = ~ apat
       { lhs = fpatlist
         rhs = fpat , fpatlist
         rhs = fpat
       { lhs = patlist
         rhs = pat , patlist
         rhs = pat
       { lhs = fpat
         rhs = qvar = pat
       { lhs = gcon
         rhs = ( )
         rhs = [ ]
         rhs = ( commalist )
         rhs = qcon
       { lhs = var
         rhs = VarId
         rhs = ( VarSym )
       { lhs = qvar
         rhs = QVarId
         rhs = VarId
         rhs = ( QVarSym )
         rhs = ( VarSym )
       { lhs = con
         rhs = ConId
         rhs = ( ConSym )
       { lhs = qcon
         rhs = QConId
         rhs = ConId
         rhs = ( : )
         rhs = ( QConSym )
         rhs = ( ConSym )
       { lhs = varop
         rhs = VarSym
         rhs = ` VarId `
       { lhs = qvarop
         rhs = QVarSym
         rhs = VarSym
         rhs = ` QVarId `
         rhs = ` VarId `
       { lhs = conop
         rhs = ConSym
         rhs = ` ConId `
       { lhs = qconop
         rhs = QConSym
         rhs = ConSym
         rhs = ` QConId `
         rhs = ` ConId `
       { lhs = op
         rhs = varop
         rhs = conop
       { lhs = qop
         rhs = qvarop
         rhs = qconop
       { lhs = gconsym
         rhs = :
         rhs = QConSym
       { lhs = tyvar
         rhs = VarId
       { lhs = tycon
         rhs = ConId
       { lhs = tycls
         rhs = ConId
       { lhs = modid
         rhs = ConId
       { lhs = qtycon
         rhs = QConId
         rhs = ConId
       { lhs = qtycls
         rhs = QConId
         rhs = ConId
       { lhs = literal
         rhs = IntLit
         rhs = FloatLit
         rhs = CharLit
         rhs = StringLit
    |];
  terminal_action = (fun _ -> 0);
}
*)
