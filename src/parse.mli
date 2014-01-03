open Batteries

(* Abstract syntax tree for Haskell 98. Copy of large chunks of Ch 3 of the
 * Report. *)
type ast = ()
(* TODO... *)

(* Build an abstract syntax tree for the given stream of lexemes. *)
val parse_ast : Lex.lexeme Queue.t -> ast

(* Terminal symbols are just lexical tokens. *)
type term = Lex.token

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
  | NTename
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
  | NTqtycon
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
