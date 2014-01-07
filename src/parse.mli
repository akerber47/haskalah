open Batteries

(* Abstract syntax tree for Haskell 98. Basically, a giant translation of the
 * Haskell grammar into ocaml. Rather than rewriting all the grammar rules /
 * AST productions as concrete variants, we're just going to use
 * polymorphic variants. This will help us with various semantic AST
 * transformations later. So this AST is not as type-safe as it *could* be
 * (could have a separate type for each sort of AST node) but this way I might
 * actually finish this year. *)

(* Comment above each AST node says what sort of children it *should* have. No
 * type system guarantees. *)
type ast0 =
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
  (* pat0 rhs *)
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
  | `Inst_con
  | `Inst_app
  | `Inst_tuple
  | `Inst_list
  | `Inst_fun
  | `Funlhs
  | `Rhs
  | `Gdrhs
  | `Gd
  | `Exp
  | `Infixexp
  | `Exp10
  | `Fexp
  | `Aexp
  | `Qual
  | `Alt
  | `Gdpat
  | `Stmt
  | `Fbind
  | `Pat
  | `Infixpat
  | `Pat10
  | `Apat
  | `Fpat
  | `Gcon
  | `Var
  | `Qvar
  | `Con
  | `Qcon
  | `Varop
  | `Qvarop
  | `Conop
  | `Qconop
  | `Op
  | `Qop
  | `Qconsym
  ]

(* Build an abstract syntax tree for the given stream of lexemes. *)
val parse : Lex.lexeme Queue.t -> ast0

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
  | NTinfixexp (* See note above *)
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
  | NTinfixpat (* See note above *)
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
  (* Above are all the non-terminals listed in the printed grammar. To actually
   * implement this grammar without wildcards in our BNF syntax, we need a few
   * more. Each bunch below listed in order of appearance on the right-hand
   * sides of the printed grammar in the Report. Luckily each sort of
   * expression generally only has one kind of separator (not different
   * separators depending on where it shows up).
   * The idea here is instead of (say) the production
   *   apat -> qcon { fpat_1 , ... , fpat_k } (k >= 0)
   * we need to use the productions
   *   apat -> qcon { apatlist }
   *   apatlist -> fpat , apatlist
   *             | e
   * so we'll just implement it like that. Basically we're expanding all the
   * wildcards, optional things, etc in the BNF grammar by hand. *)
  (* Comma-separated *)
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
  (* Semicolon-separated *)
  | NTdecllist
  | NTcdecllist
  | NTidecllist
  (* Whitespace (nothing) separated *)
  | NTatypelist
  | NTtyvarlist
  | NTbangatypelist
  | NTapatlist
