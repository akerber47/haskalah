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
   * No type system guarantees. Anything that doesn't appear as a variant here
   * (eg modid, qvar, etc) is a kind of leaf. *)
  (* Note that ALL nonterminals that can at most have a single child ast (eg
   * qop) will be "condensed" into that child (so semantic action just passes
   * it along) *)
  (* [modid] [export*] body *)
  | Ast0_module of ast0 option * (ast0 list) option * ast0
  (* topdecl* *)
  | Ast0_body of ast0 list
  (* qvar *)
  | Ast0_export_var of ast0
  (* Note that lack of cname list indicates dotdot - one or the other is
   * required by the grammar. *)
  (* qconid [qcname*] *)
  | Ast0_export_type of ast0 * (ast0 list) option
  (* modid *)
  | Ast0_export_module of ast0
  (* This one is really weird bc qualified, as, and hiding are *not* actually
   * Haskell keywords - they only work that way in import declarations. So we
   * store the things our parser *thinks* are keywords, and then check them
   * later. *)
  (* [qualified-kwd] modid [as-kwd] [as-modid] [impspec] *)
  | Ast0_topdecl_import of
    ast0 option * ast0 * (ast0 * ast0) option * ast0 option
  (* [hiding-kwd] import* *)
  | Ast0_impspec of ast0 option * ast0 list
  (* qvar *)
  | Ast0_import_var of ast0
  (* conid [qcname*] *)
  | Ast0_import_type of ast0 * (ast0 list) option
  (* simpletype type *)
  | Ast0_topdecl_type of ast0 * ast0
  (* simpletype constr* [deriving-type] *)
  | Ast0_topdecl_data of ast0 * ast0 list * ast0 option
  (* simpletype newconstr [deriving-type] *)
  | Ast0_topdecl_newtype of ast0 * ast0 * ast0 option
  (* [scontext] conid varid decl* *)
  | Ast0_topdecl_class of ast0 option * ast0 * ast0 * ast0 list
  (* [scontext] qconid inst decl* *)
  | Ast0_topdecl_instance of ast0 option * ast0 * ast0 * ast0 list
  (* type* *)
  | Ast0_topdecl_default of ast0 list
  (* decl *)
  | Ast0_topdecl_decl of ast0
  (* infixexp rhs *)
  | Ast0_decl_bind of ast0 * ast0
  (* var* type *)
  | Ast0_decl_type of ast0 list * ast0
  (* fixity [integer] op* *)
  | Ast0_decl_fixity of ast0 * ast0 option * ast0 list
  (* *)
  | Ast0_decl_empty
  (* btype type *)
  | Ast0_type_context of ast0 * ast0
  (* btype type *)
  | Ast0_type_fun of ast0 * ast0
  (* btype *)
  | Ast0_type_btype of ast0
  (* btype atype *)
  | Ast0_btype_app of ast0 * ast0
  (* atype *)
  | Ast0_btype_atype of ast0
  (* gtycon *)
  | Ast0_atype_con of ast0
  (* varid *)
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
  (* comma* *)
  | Ast0_gtycon_tuple of ast0 list
  (* simpleclass* *)
  | Ast0_scontext of ast0 list
  (* qconid varid *)
  | Ast0_simpleclass of ast0 * ast0
  (* conid varid* *)
  | Ast0_simpletype of ast0 * ast0 list
  (* btype *)
  | Ast0_constr_con of ast0
  (* btype conop btype *)
  | Ast0_constr_conop of ast0 * ast0 * ast0
  (* con fielddecl* *)
  | Ast0_constr_fields of ast0 * ast0 list
  (* con atype *)
  | Ast0_newconstr_con of ast0 * ast0
  (* con var type *)
  | Ast0_newconstr_field of ast0 * ast0 * ast0
  (* var* type *)
  | Ast0_fielddecl of ast0 list * ast0
  (* qconid* *)
  | Ast0_deriving of ast0 list
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
  (* exp [decl*] *)
  | Ast0_rhs_eq of ast0 * (ast0 list) option
  (* gdrhs [decl*] *)
  | Ast0_rhs_guard of ast0 list * (ast0 list) option
  (* infixexp exp *)
  | Ast0_gdrhs of ast0 * ast0
  (* infixexp [type] *)
  | Ast0_exp of ast0 * ast0 option
  (* exp10 qop infixexp *)
  | Ast0_infixexp_op of ast0 * ast0 * ast0
  (* exp10 *)
  | Ast0_infixexp_exp10 of ast0
  (* aexp* exp *)
  | Ast0_exp10_lambda of ast0 list * ast0
  (* decl* exp *)
  | Ast0_exp10_let of ast0 list * ast0
  (* exp exp exp *)
  | Ast0_exp10_if of ast0 * ast0 * ast0
  (* exp alt* *)
  | Ast0_exp10_case of ast0 * ast0 list
  (* stmt* *)
  | Ast0_exp10_do of ast0 list
  (* aexp* *)
  | Ast0_exp10_aexps of ast0 list
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
  (* aexp fbind* *)
  | Ast0_aexp_lbupdate of ast0 * ast0 list
  (* These next three *should* be pattern productions, but we can't tell
   * patterns and expressions apart in the parser. *)
  (* var aexp *)
  | Ast0_aexp_aspat of ast0 * ast0
  (* aexp *)
  | Ast0_aexp_irrefpat of ast0
  (* *)
  | Ast0_aexp_wildpat
  (* exp exp *)
  | Ast0_qual_assign of ast0 * ast0
  (* decl* *)
  | Ast0_qual_let of ast0 list
  (* exp *)
  | Ast0_qual_guard of ast0
  (* exp exp [decl*] *)
  | Ast0_alt_match of ast0 * ast0 * (ast0 list) option
  (* exp gdpat* [decl*] *)
  | Ast0_alt_guard of ast0 * ast0 list * (ast0 list) option
  (* gd exp *)
  | Ast0_gdpat of ast0 * ast0
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
  (* *)
  | Ast0_gcon_unit
  | Ast0_gcon_list
  (* int *)
  | Ast0_gcon_tuple of ast0 list
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
  | Ast0_parenthesized_leaf of lexeme
  | Ast0_backquoted_leaf of lexeme
  | Ast0_leaf of lexeme
  (* This AST node is produced by ALL the NT<stuff>list productions.
   * NT<stuff>list => <stuff>*
   * Basically this is a trick to cut down on how many AST nodes we need,
   * rather than building a huge "linked-list-shaped-tree" inside our AST each
   * time we encounter a NT<stuff>list production.
   * This node should *not* be present in the final AST - contents should be
   * copied out by higher-level node once list is complete. *)
  | Ast0_partial_list of ast0 list

(* Lex error: index in input string where it occurred, and error message. *)
exception Lex_error of int * string
;;

(* Parse error: error message *)
exception Parse_error of string
;;
