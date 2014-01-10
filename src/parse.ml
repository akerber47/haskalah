open Batteries
;;
open Types
;;

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
    type tm = token
    let tm_compare = compare
    type ntm = nonterm
    let ntm_compare = compare
    type lx = lexeme
    let lx_to_tm = (fun lx -> lx.token )
    let eof = EOF
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
       { lhs = NTstmtlist;
         rhs = [ NT NTstmt; T Semicolon; NT NTstmtlist ];
         semantic_action = (fun _ -> 0);};
       { lhs = NTstmtlist;
         rhs = [ NT NTstmt ];
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
