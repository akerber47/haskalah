open Batteries

(* Abstract syntax tree for Haskell 98. Basically, a giant translation of the
 * Haskell grammar into ocaml types. *)

type tmodule = {
  id : tmodid option;
  exports : (texport list) option;
  body : tbody;
}

and tbody = {
  imports : (timpdecl list) option;
  decls : (ttopdecl list) option;
}

and texport =
  | Texport_var of tqvar
  | Texport_type of texport_type
  | Texport_class of texport_class
  | Texport_module of tmodid

and texport_type = {
  name : tqtycon;
  cons : (tcname list) option;
}

and texport_class = {
  name : tqtycls;
  methods : (tqvar list) option;
}

and timpdecl = {
  qualified : bool;
  id : tmodid;
  as_id : tmodid option;
  hiding : bool;
  spec : (timport list) option;
}

and timport =
  | Timport_var of tvar
  | Timport_type of timport_type
  | Timport_class of timport_class
and timport_type = {
  name : ttycon;
  cons : (tcname list) option;
}
and timport_class = {
  name : ttycls;
  methods : (tvar list) option;
}

and tcname =
  | Tcname_var of tvar
  | Tcname_con of tcon

and ttopdecl =
  | Ttopdecl_type of ttopdecl_type
  | Ttopdecl_data of ttopdecl_data
  | Ttopdecl_newtype of ttopdecl_newtype
  | Ttopdecl_class of ttopdecl_class
  | Ttopdecl_instance of ttopdecl_instance
  | Ttopdecl_default of ttype list
  | Ttopdecl_decl of tdecl
and ttopdecl_type = {
  ttopdecl_type_lhs : tsimpletype;
  ttopdecl_type_rhs : ttype;
}
and ttopdecl_data = {
  context : tcontext;
  lhs : tsimpletype;
  rhs : tconstr list;
  deriving : tderiving option;
}
and ttopdecl_newtype = {
  context : tcontext;
  lhs : tsimpletype;
  rhs : tnewconstr;
  deriving : tderiving option;
}
and ttopdecl_class = {
  context : tscontext;
  name : ttycls;
  var : ttyvar;
  where_body : (tcdecl list) option;
}
and ttopdecl_instance = {
  context : tscontext;
  name : ttycls;
  inst : tinst;
  where_body : (tidecl list) option;
}

and tdecl =
  | Tdecl_general of tgendecl
  | Tdecl_pat of tdecl_pat
  | Tdecl_fun of tdecl_fun
and tdecl_pat = {
  lhs : tpat0;
  rhs : trhs;
}
and tdecl_fun = {
  lhs : tfunlhs;
  rhs : trhs;
}



type ast =
  | ASTmodule of tmodule
  | ASTbody of tbody
  | ASTexport of texport
  | ASTimpdecl of timpdecl
  | ASTimport of timport
  | ASTcname of tcname
  | ASTtopdecl of ttopdecl
  | ASTdecl of tdecl
  | ASTcdecl of tcdecl
  | ASTidecl of tidecl
  | ASTgendecl of tgendecl
  | ASTfixity of tfixity
  | ASTtype of ttype
  | ASTbtype of tbtype
  | ASTatype of tatype
  | ASTqtycon of tqtycon
  | ASTcontext of tcontext
  | ASTclass of tclass
  | ASTscontext of tscontext
  | ASTsimpleclass of tsimpleclass
  | ASTsimpletype of tsimpletype
  | ASTconstr of tconstr
  | ASTnewconstr of tnewconstr
  | ASTfielddecl of tfielddecl
  | ASTderiving of tderiving
  | ASTdclass of tdclass
  | ASTinst of tinst
  | ASTfunlhs of tfunlhs
  | ASTrhs of trhs
  | ASTgdrhs of tgdrhs
  | ASTgd of tgd
  | ASTexp of texp
  | ASTinfixexp of tinfixexp
  | ASTexp10 of texp10
  | ASTfexp of tfexp
  | ASTaexp of taexp
  | ASTqual of tqual
  | ASTalt of talt
  | ASTgdpat of tgdpat
  | ASTstmt of tstmt
  | ASTfbind of tfbind
  | ASTpat of tpat
  | ASTinfixpat of tinfixpat
  | ASTpat10 of tpat10
  | ASTapat of tapat
  | ASTfpat of tfpat
  | ASTgcon of tgcon
  | ASTvar of tvar
  | ASTqvar of tqvar
  | ASTcon of tcon
  | ASTqcon of tqcon
  | ASTvarop of tvarop
  | ASTqvarop of tqvarop
  | ASTconop of tconop
  | ASTqconop of tqconop
  | ASTop of top
  | ASTqop of tqop
  | ASTqconsym of tqconsym

(* Build an abstract syntax tree for the given stream of lexemes. *)
val parse : Lex.lexeme Queue.t -> ast

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
