(* This file contains types used in multiple phases of the compiler. To avoid
 * horrible copy-paste duplication, it's symlinked with its implementation
 * (since there are only types in here, that works fine) *)

(* Token types used by 1st pass of lexer *)
type pretoken =
  | PreQVarId
  | PreQVarSym
  | PreIntLit
  | PreFloatLit
  | PreCharLit
  | PreStringLit
  | PreSpecial

type prelexeme = {
  pretoken  : pretoken;
  startix   : int;    (* Starting index in the raw (unsplit) source string *)
  endix     : int;    (* Ending index, points *after* last char. *)
}

(* Token types of lexer output, passed to parser *)
type token =
  | EOF
  | VarId
  | ConId
  | VarSym
  | ConSym
  (* These only match names that are *actually* qualified *)
  | QVarId
  | QConId
  | QVarSym
  | QConSym
  | IntLit
  | FloatLit
  | CharLit
  | StringLit
  (* Reserved words *)
  | RCase | RClass | RData | RDefault | RDeriving | RDo | RElse | RIf | RImport
  | RIn | RInfix | RInfixl | RInfixr | RInstance | RLet | RModule | RNewtype
  | ROf | RThen | RType | RWhere | RUnderscore
  (* Reserved operators *)
  | RDotDot | RColon | RColonColon | REquals | RBackslash | RPipe | RLArrowDash
  | RDashRArrow | RAt | RTilde | REqualsRArrow
  (* Special characters *)
  | LParen | RParen | LSquare | RSquare | LCurly | RCurly
  | Comma | Semicolon | Backquote

(* Lexer output / parser input is a lexeme Queue.t *)
type lexeme = {
  token     : token;
  contents  : string;
  startraw  : int;    (* Starting index in the raw (unsplit) source string *)
  endraw    : int;
}

(* Nonterminal symbols in the Haskell 98 grammar. Long, ugly verbatim copy of
 * section 9.5 of the Report. Note that we implement
 * https://ghc.haskell.org/trac/haskell-prime/wiki/FixityResolution ie resolve
 * precedence and fixities of operator expressions in a later step. This
 * violates the Haskell 98 specification. *)
type nonterm =
  | Goal
  | NTmodule
  | NTbody
  | NTexports
  | NTexport
  | NTimpspec
  | NTimport
  | NTqcname
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
  | NTqvars
  | NTfixity
  | NTtype
  | NTbtype
  | NTatype
  | NTgtycon
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
  | NTrhs
  | NTgdrhs
  | NTgd
  | NTexp
  | NTinfixexp (* see note above *)
  | NTexp10
  | NTaexp
  | NTqual
  | NTalts
  | NTalt
  | NTgdpat
  | NTstmts
  | NTstmt
  | NTfbind
  | NTgcon
  | NTqvar
  | NTcon
  | NTqcon
  | NTqvarop
  | NTconop
  | NTqconop
  | NTop
  | NTqop
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
  | NTexportlist
  | NTqcnamelist
  | NTimportlist
  | NTtopdecllist
  | NTqvarlist
  | NTtypelist
  | NTcommalist
  | NToplist
  | NTsimpleclasslist
  | NTfielddecllist
  | NTdclasslist
  | NTexplist
  | NTquallist
  | NTfbindlist
  | NTtyvarcommalist
  (* Semicolon-separated *)
  | NTdecllist
  | NTcdecllist
  | NTidecllist
  | NTaltlist
  (* Pipe separated *)
  | NTconstrlist
  (* Whitespace (nothing) separated *)
  | NTtyvarlist
  | NTaexplist
  | NTstmtlist
  | NTqvarid (* For an optionally qualified varid (matches VarId or QVarId *)
  | NTqconid
  | NTliteral

(* States and tables of the parsing pushdown automaton. State numbers
 * correspond to various structures in the parser generator internals. These
 * are really only included here to avoid circular type dependencies between
 * the parser simulator and the computed parser generator tables. *)
type state = int

type action =
  | Shift of state
  (* Store index of production (in grammar list) we use to reduce. *)
  | Reduce of int
  (* Store index of goal production we use to accept *)
  | Accept of int

(* Output of parser *)
type ast0 = {
  node : ast0node;
  (* For error reporting purposes, start and end of this syntax block in
   * source. Basically just min/max over all tokens that make up the block.
   * (ignoring implicitly generated tokens) *)
  blockstart : int;
  blockend   : int;
}

(* Abstract syntax tree for Haskell 98. Basically, a giant translation of the
 * Haskell grammar into ocaml. Rather than having a different
 * type for each grammar node, we're just going to use variants.
 * This will make it easy to perform various AST transformations later and to
 * avoid horrific combinations of types. Note that polymorphic variants don't
 * help me here anyways.
 * So this AST is nowhere near as type-safe as it *could* be (could have
 * a separate type for each sort of AST node) but this way I might actually
 * finish this year. *)

and ast0node =
  (* Comment above each AST node says what sort of children it *should* have.
   * No type system guarantees. *)
  (* [modid] [export*] body *)
  | Ast0_module of ast0 option * (ast0 list) option * ast0
  (* topdecl* *)
  | Ast0_body of ast0 list
  (* qvar *)
  | Ast0_export_var of ast0
  (* qtycon [cname*] *)
  | Ast0_export_type of ast0 * (ast0 list) option
  (* qtycls [qvar*] *)
  | Ast0_export_class of ast0 * (ast0 list) option
  (* modid *)
  | Ast0_export_module of ast0
  (* qualified? modid [modid] hiding? import* *)
  | Ast0_impdecl of bool * ast0 * (ast0 option) * bool * (ast0 list)
  (* var *)
  | Ast0_import_var of ast0
  (* tycon [cname*] *)
  | Ast0_import_type of ast0 * (ast0 list) option
  (* tycls [var*] *)
  | Ast0_import_class of ast0 * (ast0 list) option
  (* var *)
  | Ast0_cname_var of ast0
  (* con *)
  | Ast0_cname_con of ast0
  (* simpletype type *)
  | Ast0_topdecl_type of ast0 * ast0
  (* NOTE no datatype contexts in our grammar *)
  (* simpletype constr* [deriving] *)
  | Ast0_topdecl_data of ast0 * ast0 list * ast0 option
  (* simpletype newconstr [deriving] *)
  | Ast0_topdecl_newtype of ast0 * ast0 * ast0 option
  (* [scontext] tycls tyvar cdecl* *)
  | Ast0_topdecl_class of ast0 option * ast0 * ast0 * ast0 list
  (* [scontext] qtycls inst idecl* *)
  | Ast0_topdecl_instance of ast0 option * ast0 * ast0 * ast0 list
  (* type* *)
  | Ast0_topdecl_default of ast0 list
  (* decl *)
  | Ast0_topdecl_decl of ast0
  (* gendecl *)
  | Ast0_decl_general of ast0
  (* funlhs rhs *)
  | Ast0_decl_fun of ast0 * ast0
  (* infixpat rhs *)
  | Ast0_decl_pat of ast0 * ast0
  (* gendecl *)
  | Ast0_cdecl_general of ast0
  (* funlhs rhs *)
  | Ast0_cdecl_fun of ast0 * ast0
  (* var rhs *)
  | Ast0_cdecl_var of ast0 * ast0
  (* funlhs rhs *)
  | Ast0_idecl_fun of ast0 * ast0
  (* var rhs *)
  | Ast0_idecl_var of ast0 * ast0
  (* *)
  | Ast0_idecl_empty
  (* var* [context] type *)
  | Ast0_gendecl_type of ast0 list * ast0 option * ast0
  (* fixity [integer] [op] *)
  | Ast0_gendecl_fixity of ast0 * ast0 option * ast0 list
  (* *)
  | Ast0_gendecl_empty
  | Ast0_fixity_left
  | Ast0_fixity_right
  | Ast0_fixity_none
  (* btype [atype] *)
  | Ast0_type of ast0 * ast0 option
  (* [btype] atype *)
  | Ast0_btype of ast0 option * ast0
  (* gtycon *)
  | Ast0_atype_con of ast0
  (* tyvar *)
  | Ast0_atype_var of ast0
  (* type* *)
  | Ast0_atype_tuple of ast0 list
  (* type *)
  | Ast0_atype_list of ast0
  (* type *)
  | Ast0_atype_paren of ast0
  (* qtycon *)
  | Ast0_gtycon_con of ast0
  | Ast0_gtycon_unit
  | Ast0_gtycon_list
  | Ast0_gtycon_fun
  | Ast0_gtycon_tuple of int
  (* class* *)
  | Ast0_context of ast0 list
  (* qtycls tyvar *)
  | Ast0_class_simple of ast0 * ast0
  (* qtycls tyvar atype* *)
  | Ast0_class_complex of ast0 * ast0 * ast0 list
  (* simpleclass* *)
  | Ast0_scontext of ast0 list
  (* qtycls tyvar *)
  | Ast0_simpleclass of ast0 * ast0
  (* tycon tyvar* *)
  | Ast0_simpletype of ast0 * ast0 list
  (* con (atype, strict?)* *)
  | Ast0_constr_con of ast0 * (ast0 * bool) list
  (* btype/atype strict? conop btype/atype strict? *)
  | Ast0_constr_conop of ast0 * bool * ast0 * ast0 * bool
  (* con fielddecl* *)
  | Ast0_constr_fields of ast0 * ast0 list
  (* con atype *)
  | Ast0_newconstr_con of ast0
  (* con var type *)
  | Ast0_newconstr_field of ast0 * ast0 * ast0
  (* var* type/atype strict? *)
  | Ast0_fielddecl of ast0 list * ast0 * bool
  (* dclass* *)
  | Ast0_deriving of ast0 list
  (* qtycls *)
  | Ast0_dclass of ast0
  (* gtycon *)
  | Ast0_inst_con of ast0
  (* gtycon tyvar* *)
  | Ast0_inst_app of ast0 * ast0 list
  (* tyvar* *)
  | Ast0_inst_tuple of ast0 list
  (* tyvar *)
  | Ast0_inst_list of ast0
  (* tyvar tyvar *)
  | Ast0_inst_fun of ast0 * ast0
  (* var apat* *)
  | Ast0_funlhs_var of ast0 * ast0 list
  (* infixpat *)
  | Ast0_funlhs_pat of ast0
  (* funlhs apat* *)
  | Ast0_funlhs_app of ast0 * ast0 list
  (* exp [decl*] *)
  | Ast0_rhs_exp of ast0 * (ast0 list) option
  (* gdrhs [decl*] *)
  | Ast0_rhs_guard of ast0 * (ast0 list) option
  (* gd exp [gdrhs] *)
  | Ast0_gdrhs of ast0 * ast0 * (ast0 list) option
  (* infixexp *)
  | Ast0_gd of ast0
  (* infixexp [context] type *)
  | Ast0_exp_typed of ast0 * ast0 option * ast0
  (* infixexp *)
  | Ast0_exp_infix of ast0
  (* exp10 qop infixexp *)
  | Ast0_infixexp_op of ast0 * ast0 * ast0
  (* infixexp *)
  | Ast0_infixexp_negate of ast0
  (* exp10 *)
  | Ast0_infixexp_exp10 of ast0
  (* apat* exp *)
  | Ast0_exp10_lambda of ast0 list * ast0
  (* decl* exp *)
  | Ast0_exp10_let of ast0 list * ast0
  (* exp exp exp *)
  | Ast0_exp10_if of ast0 * ast0 * ast0
  (* exp alt* *)
  | Ast0_exp10_case of ast0 * ast0 list
  (* stmt* *)
  | Ast0_exp10_do of ast0 list
  (* fexp *)
  | Ast0_exp10_exp of ast0
  (* [fexp] aexp *)
  | Ast0_fexp of ast0 option * ast0
  (* qvar *)
  | Ast0_aexp_var of ast0
  (* gcon *)
  | Ast0_aexp_con of ast0
  (* literal *)
  | Ast0_aexp_literal of ast0
  (* exp *)
  | Ast0_aexp_paren of ast0
  (* exp* *)
  | Ast0_aexp_tuple of ast0 list
  (* exp* *)
  | Ast0_aexp_list of ast0 list
  (* exp [exp] [exp] *)
  | Ast0_aexp_seq of ast0 * ast0 option * ast0 option
  (* exp qual* *)
  | Ast0_aexp_comp of ast0 * ast0 list
  (* infixexp qop *)
  | Ast0_aexp_lsec of ast0 * ast0
  (* qop infixexp *)
  | Ast0_aexp_rsec of ast0 * ast0
  (* qop fbind* *)
  | Ast0_aexp_labelcon of ast0 * ast0 list
  (* aexp fbind* *)
  | Ast0_aexp_labelupdate of ast0 * ast0 list
  (* pat exp *)
  | Ast0_qual_gen of ast0 * ast0
  (* decl* *)
  | Ast0_qual_let of ast0 list
  (* exp *)
  | Ast0_qual_guard of ast0
  (* pat exp [decl*] *)
  | Ast0_alt_pat of ast0 * ast0 * (ast0 list) option
  (* pat gdpat [decl*] *)
  | Ast0_alt_guard of ast0 * ast0 * (ast0 list) option
  (* gd exp [gdpat] *)
  | Ast0_gdpat of ast0 * ast0 * ast0 option
  (* exp *)
  | Ast0_stmt_exp of ast0
  (* pat exp *)
  | Ast0_stmt_assign of ast0 * ast0
  (* decl* *)
  | Ast0_stmt_let of ast0 list
  (* *)
  | Ast0_stmt_empty
  (* qvar exp *)
  | Ast0_fbind of ast0 * ast0
  (* NOTE no n+k patterns in our grammar *)
  (* infixpat *)
  | Ast0_pat of ast0
  (* pat10 qconop infixpat *)
  | Ast0_infixpat_op of ast0 * ast0 * ast0
  (* - (integer | Ast0_float) *)
  | Ast0_infixpat_negate of ast0
  (* pat10 *)
  | Ast0_infixpat_pat10 of ast0
  (* apat *)
  | Ast0_pat10_pat of ast0
  (* gcon apat* *)
  | Ast0_pat10_conapp of ast0 * ast0 list
  (* var apat *)
  | Ast0_apat_as of ast0 * (ast0 option)
  (* gcon *)
  | Ast0_apat_nullary of ast0
  (* qcon fpat* *)
  | Ast0_apat_labeled of ast0 * ast0 list
  (* literal *)
  | Ast0_apat_literal of ast0
  (* *)
  | Ast0_apat_wildcard
  (* pat *)
  | Ast0_apat_paren of ast0
  (* pat* *)
  | Ast0_apat_tuple of ast0 list
  (* pat* *)
  | Ast0_apat_list of ast0 list
  (* pat *)
  | Ast0_apat_irref of ast0
  (* qvar pat *)
  | Ast0_fpat of ast0 * ast0
  (* *)
  | Ast0_gcon_unit
  | Ast0_gcon_list
  (* int *)
  | Ast0_gcon_tuple of int
  (* qcon *)
  | Ast0_gcon_qcon of ast0
  (* All of these below simply store a single id / symbol (or :), so
   * will be represented by leaf, with the lexeme storing the remaining
   * (actual) type data needed.
   * in lexical syntax:
   * varid, conid, tyvar, tyvar, tycon, tycls, modid, qvarid, qconid, qtycon,
   * qtycls, qvarsym, qconsym, literal.
   * in context-free syntax:
   * var, qvar, con, qcon, varop, qvarop, conop, qconop, op, qop, gconsym. *)
  | Ast0_leaf of lexeme
  (* This AST node is produced by ALL the NT<stuff>list productions.
   * NT<stuff>list => <stuff>*
   * Basically this is a trick to cut down on how many AST nodes we need. *)
  | Ast0_partial_list of ast0 list

(* Lex error: index in input string where it occurred, and error message. *)
exception Lex_error of int * string
;;

(* Parse error: error message *)
exception Parse_error of string
;;
