(* This program generates the Haskell action/goto table source file *)

open Batteries
;;
open Types
;;

module Haskell_pg = Parser_gen.Make_gen (
  struct
    type tm = token
    type ntm = nonterm
    let eof = Types.EOF
    let tm_print = Print.token_print
    let ntm_print = Print.nonterm_print
  end
);;

open Haskell_pg
;;

(* This is just the acfg from Parse. Look there for explanatory comments. *)
let haskell_cfg = {
  goal = Goal;
  productions =
    [| { lhs = Goal;
         rhs = [ NT NTmodule ]; };

       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; T RWhere; NT NTbody ]; };

       { lhs = NTmodule;
         rhs = [ T RModule; T ConId; NT NTexports; T RWhere; NT NTbody ]; };

       { lhs = NTmodule;
         rhs = [ NT NTbody ]; };

       { lhs = NTbody;
         rhs = [ T LCurly; NT NTtopdecls; T RCurly ]; };

       { lhs = NTexports;
         rhs = [ T LParen; T RParen ]; };

       { lhs = NTexports;
         rhs = [ T LParen; T Comma; T RParen ]; };

       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T RParen ]; };

       { lhs = NTexports;
         rhs = [ T LParen; NT NTexportlist; T Comma; T RParen ]; };

       { lhs = NTexportlist;
         rhs = [ NT NTexport; T Comma; NT NTexportlist ]; };

       { lhs = NTexportlist;
         rhs = [ NT NTexport ]; };

       { lhs = NTexport;
         rhs = [ NT NTqvar ]; };

       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; T RDotDot; T RParen ]; };

       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; T RParen ]; };

       { lhs = NTexport;
         rhs = [ NT NTqconid; T LParen; NT NTqcnamelist; T RParen ]; };

       { lhs = NTexport;
         rhs = [ T RModule; T ConId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; T VarId; T ConId; NT NTimpspec ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; T VarId; T ConId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId; NT NTimpspec ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T VarId; T ConId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; T VarId; T ConId; NT NTimpspec ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; T VarId; T ConId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId; NT NTimpspec ]; };

       { lhs = NTtopdecl;
         rhs = [ T RImport; T ConId ]; };

       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; T Comma; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T VarId; T LParen; NT NTimportlist; T Comma; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T LParen; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T LParen; T Comma; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T RParen ]; };

       { lhs = NTimpspec;
         rhs = [ T LParen; NT NTimportlist; T Comma; T RParen ]; };

       { lhs = NTimportlist;
         rhs = [ NT NTimport; T Comma; NT NTimportlist ]; };

       { lhs = NTimportlist;
         rhs = [ NT NTimport ]; };

       { lhs = NTimport;
         rhs = [ NT NTqvar ]; };

       { lhs = NTimport;
         rhs = [ T ConId; T LParen; T RDotDot; T RParen ]; };

       { lhs = NTimport;
         rhs = [ T ConId; T LParen; T RParen ]; };

       { lhs = NTimport;
         rhs = [ T ConId; T LParen; NT NTqcnamelist; T RParen ]; };

       { lhs = NTqcname;
         rhs = [ NT NTqvar ]; };

       { lhs = NTqcname;
         rhs = [ NT NTqcon ]; };

       { lhs = NTtopdecls;
         rhs = [ NT NTtopdecllist ]; };

       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl; T Semicolon; NT NTtopdecllist ]; };

       { lhs = NTtopdecllist;
         rhs = [ NT NTtopdecl ]; };

       { lhs = NTtopdecl;
         rhs = [ T RType; NT NTsimpletype; T REquals; NT NTtype ]; };

       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs; NT NTderiving ]; };

       { lhs = NTtopdecl;
         rhs = [ T RData; NT NTsimpletype; T REquals; NT NTconstrs ]; };

       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr; NT NTderiving ]; };

       { lhs = NTtopdecl;
         rhs = [ T RNewtype; NT NTsimpletype; T REquals; NT NTnewconstr ]; };

       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; T ConId; T VarId; T RWhere; NT NTdecls ]; };

       { lhs = NTtopdecl;
         rhs = [ T RClass; NT NTscontext; T REqualsRArrow; T ConId; T VarId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RClass; T ConId; T VarId; T RWhere; NT NTdecls ]; };

       { lhs = NTtopdecl;
         rhs = [ T RClass; T ConId; T VarId ]; };

       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqconid; NT NTinst; T RWhere; NT NTdecls ]; };

       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTscontext; T REqualsRArrow; NT NTqconid; NT NTinst ]; };

       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqconid; NT NTinst; T RWhere; NT NTdecls ]; };

       { lhs = NTtopdecl;
         rhs = [ T RInstance; NT NTqconid; NT NTinst ]; };

       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; T RParen ]; };

       { lhs = NTtopdecl;
         rhs = [ T RDefault; T LParen; NT NTtypelist; T RParen ]; };

       { lhs = NTtopdecl;
         rhs = [ NT NTdecl ]; };

       { lhs = NTtypelist;
         rhs = [ NT NTtype; T Comma; NT NTtypelist ]; };

       { lhs = NTtypelist;
         rhs = [ NT NTtype ]; };

       { lhs = NTdecls;
         rhs = [ T LCurly; T RCurly ]; };

       { lhs = NTdecls;
         rhs = [ T LCurly; NT NTdecllist; T RCurly ]; };

       { lhs = NTdecllist;
         rhs = [ NT NTdecl; T Semicolon; NT NTdecllist ]; };

       { lhs = NTdecllist;
         rhs = [ NT NTdecl ]; };

       { lhs = NTdecl;
         rhs = [ NT NTgendecl ]; };

       { lhs = NTdecl;
         rhs = [ NT NTinfixexp; NT NTrhs ]; };

       { lhs = NTgendecl;
         rhs = []; };

       { lhs = NTgendecl;
         rhs = [ NT NTqvars; T RColonColon; NT NTtype ]; };

       { lhs = NTgendecl;
         rhs = [ NT NTfixity; T IntLit; NT NTops ]; };

       { lhs = NTgendecl;
         rhs = [ NT NTfixity; NT NTops ]; };

       { lhs = NTops;
         rhs = [ NT NToplist ]; };

       { lhs = NToplist;
         rhs = [ NT NTop; T Comma; NT NToplist ]; };

       { lhs = NToplist;
         rhs = [ NT NTop ]; };

       { lhs = NTqvars;
         rhs = [ NT NTqvarlist ]; };

       { lhs = NTqvarlist;
         rhs = [ NT NTqvar; T Comma; NT NTqvarlist ]; };

       { lhs = NTqvarlist;
         rhs = [ NT NTqvar ]; };

       { lhs = NTfixity;
         rhs = [ T RInfixl ]; };

       { lhs = NTfixity;
         rhs = [ T RInfixr ]; };

       { lhs = NTfixity;
         rhs = [ T RInfix ]; };

       { lhs = NTtype;
         rhs = [ NT NTbtype; T RDashRArrow; NT NTtype ]; };

       { lhs = NTtype;
         rhs = [ NT NTbtype; T REqualsRArrow; NT NTtype ]; };

       { lhs = NTtype;
         rhs = [ NT NTbtype ]; };

       { lhs = NTbtype;
         rhs = [ NT NTbtype; NT NTatype ]; };

       { lhs = NTbtype;
         rhs = [ NT NTatype ]; };

       { lhs = NTatype;
         rhs = [ NT NTgtycon ]; };

       { lhs = NTatype;
         rhs = [ T VarId ]; };

       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T Comma; NT NTtypelist; T RParen ]; };

       { lhs = NTatype;
         rhs = [ T LSquare; NT NTtype; T RSquare ]; };

       { lhs = NTatype;
         rhs = [ T LParen; NT NTtype; T RParen ]; };

       { lhs = NTgtycon;
         rhs = [ NT NTqconid ]; };

       { lhs = NTgtycon;
         rhs = [ T LParen; T RParen ]; };

       { lhs = NTgtycon;
         rhs = [ T LSquare; T RSquare ]; };

       { lhs = NTgtycon;
         rhs = [ T LParen; T RDashRArrow; T RParen ]; };

       { lhs = NTgtycon;
         rhs = [ T LParen; NT NTcommalist; T RParen ]; };

       { lhs = NTcommalist;
         rhs = [ T Comma; NT NTcommalist ]; };

       { lhs = NTcommalist;
         rhs = [ T Comma ]; };

       { lhs = NTscontext;
         rhs = [ NT NTsimpleclass ]; };

       { lhs = NTscontext;
         rhs = [ T LParen; T RParen ]; };

       { lhs = NTscontext;
         rhs = [ T LParen; NT NTsimpleclasslist; T RParen ]; };

       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass; T Comma; NT NTsimpleclasslist ]; };

       { lhs = NTsimpleclasslist;
         rhs = [ NT NTsimpleclass ]; };

       { lhs = NTsimpleclass;
         rhs = [ NT NTqconid; T VarId ]; };

       { lhs = NTsimpletype;
         rhs = [ T ConId ]; };

       { lhs = NTsimpletype;
         rhs = [ T ConId; NT NTtyvarlist ]; };

       { lhs = NTtyvarlist;
         rhs = [ T VarId; NT NTtyvarlist ]; };

       { lhs = NTtyvarlist;
         rhs = [ T VarId ]; };

       { lhs = NTconstrs;
         rhs = [ NT NTconstrlist ]; };

       { lhs = NTconstrlist;
         rhs = [ NT NTconstr; T RPipe; NT NTconstrlist ]; };

       { lhs = NTconstrlist;
         rhs = [ NT NTconstr ]; };

       { lhs = NTconstr;
         rhs = [ NT NTbtype ]; };

       { lhs = NTconstr;
         rhs = [ NT NTbtype; NT NTconop; NT NTbtype ]; };

       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; T RCurly ]; };

       { lhs = NTconstr;
         rhs = [ NT NTcon; T LCurly; NT NTfielddecllist; T RCurly ]; };

       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl; T Comma; NT NTfielddecllist ]; };

       { lhs = NTfielddecllist;
         rhs = [ NT NTfielddecl ]; };

       { lhs = NTnewconstr;
         rhs = [ NT NTcon; NT NTatype ]; };

       { lhs = NTnewconstr;
         rhs = [ NT NTcon; T LCurly; NT NTqvar; T RColonColon; NT NTtype; T RCurly ]; };

       { lhs = NTfielddecl;
         rhs = [ NT NTqvars; T RColonColon; NT NTtype ]; };

       { lhs = NTderiving;
         rhs = [ T RDeriving; NT NTdclass ]; };

       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; T RParen ]; };

       { lhs = NTderiving;
         rhs = [ T RDeriving; T LParen; NT NTdclasslist; T RParen ]; };

       { lhs = NTdclasslist;
         rhs = [ NT NTdclass; T Comma; NT NTdclasslist ]; };

       { lhs = NTdclasslist;
         rhs = [ NT NTdclass ]; };

       { lhs = NTdclass;
         rhs = [ NT NTqconid ]; };

       { lhs = NTinst;
         rhs = [ NT NTgtycon ]; };

       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; T RParen ]; };

       { lhs = NTinst;
         rhs = [ T LParen; NT NTgtycon; NT NTtyvarlist; T RParen ]; };

       { lhs = NTinst;
         rhs = [ T LParen; T VarId; T Comma; NT NTtyvarcommalist; T RParen ]; };

       { lhs = NTinst;
         rhs = [ T LSquare; T VarId; T RSquare ]; };

       { lhs = NTinst;
         rhs = [ T LParen; T VarId; T RDashRArrow; T VarId; T RParen ]; };

       { lhs = NTtyvarcommalist;
         rhs = [ T VarId; T Comma; NT NTtyvarcommalist ]; };

       { lhs = NTtyvarcommalist;
         rhs = [ T VarId ]; };

       { lhs = NTaexplist;
         rhs = [ NT NTaexp; NT NTaexplist ]; };

       { lhs = NTaexplist;
         rhs = [ NT NTaexp ]; };

       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp; T RWhere; NT NTdecls ]; };

       { lhs = NTrhs;
         rhs = [ T REquals; NT NTexp ]; };

       { lhs = NTrhs;
         rhs = [ NT NTgdrhs; T RWhere; NT NTdecls ]; };

       { lhs = NTrhs;
         rhs = [ NT NTgdrhs ]; };

       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp; NT NTgdrhs ]; };

       { lhs = NTgdrhs;
         rhs = [ NT NTgd; T REquals; NT NTexp ]; };

       { lhs = NTgd;
         rhs = [ T RPipe; NT NTinfixexp ]; };

       { lhs = NTexp;
         rhs = [ NT NTinfixexp; T RColonColon; NT NTtype ]; };

       { lhs = NTexp;
         rhs = [ NT NTinfixexp ]; };

       { lhs = NTinfixexp;
         rhs = [ NT NTexp10; NT NTqop; NT NTinfixexp ]; };

       { lhs = NTinfixexp;
         rhs = [ NT NTexp10 ]; };

       { lhs = NTexp10;
         rhs = [ T RBackslash; NT NTaexplist; T RDashRArrow; NT NTexp ]; };

       { lhs = NTexp10;
         rhs = [ T RLet; NT NTdecls; T RIn; NT NTexp ]; };

       { lhs = NTexp10;
         rhs = [ T RIf; NT NTexp; T RThen; NT NTexp; T RElse; NT NTexp ]; };

       { lhs = NTexp10;
         rhs = [ T RCase; NT NTexp; T ROf; T LCurly; NT NTalts; T RCurly ]; };

       { lhs = NTexp10;
         rhs = [ T RDo; T LCurly; NT NTstmts; T RCurly ]; };

       { lhs = NTexp10;
         rhs = [ NT NTaexplist ]; };

       { lhs = NTaexp;
         rhs = [ NT NTqvar ]; };

       { lhs = NTaexp;
         rhs = [ NT NTgcon ]; };

       { lhs = NTaexp;
         rhs = [ NT NTliteral ]; };

       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T RParen ]; };

       { lhs = NTaexp;
         rhs = [ T LParen; NT NTexp; T Comma; NT NTexplist; T RParen ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexplist; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; NT NTexp; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T Comma; NT NTexp; T RDotDot; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; NT NTexp; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RDotDot; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LSquare; NT NTexp; T RPipe; NT NTquallist; T RSquare ]; };

       { lhs = NTaexp;
         rhs = [ T LParen; NT NTinfixexp; NT NTqop; T RParen ]; };

       { lhs = NTaexp;
         rhs = [ T LParen; NT NTqop; NT NTinfixexp; T RParen ]; };

       { lhs = NTaexp;
         rhs = [ NT NTaexp; T LCurly; T RCurly ]; };

       { lhs = NTaexp;
         rhs = [ NT NTaexp; T LCurly; NT NTfbindlist; T RCurly ]; };

       { lhs = NTaexp;
         rhs = [ NT NTqvar; T RAt; NT NTaexp ]; };

       { lhs = NTaexp;
         rhs = [ T RTilde; NT NTaexp ]; };

       { lhs = NTaexp;
         rhs = [ T RUnderscore ]; };

       { lhs = NTexplist;
         rhs = [ NT NTexp; T Comma; NT NTexplist ]; };

       { lhs = NTexplist;
         rhs = [ NT NTexp ]; };

       { lhs = NTquallist;
         rhs = [ NT NTqual; T Comma; NT NTquallist ]; };

       { lhs = NTquallist;
         rhs = [ NT NTqual ]; };

       { lhs = NTfbindlist;
         rhs = [ NT NTfbind; T Comma; NT NTfbindlist ]; };

       { lhs = NTfbindlist;
         rhs = [ NT NTfbind ]; };

       { lhs = NTqual;
         rhs = [ NT NTexp; T RLArrowDash; NT NTexp ]; };

       { lhs = NTqual;
         rhs = [ T RLet; NT NTdecls ]; };

       { lhs = NTqual;
         rhs = [ NT NTexp ]; };

       { lhs = NTalts;
         rhs = [ NT NTaltlist ]; };

       { lhs = NTaltlist;
         rhs = [ NT NTalt; T Semicolon; NT NTaltlist ]; };

       { lhs = NTaltlist;
         rhs = [ NT NTalt ]; };

       { lhs = NTalt;
         rhs = [ NT NTexp; T RDashRArrow; NT NTexp; T RWhere; NT NTdecls ]; };

       { lhs = NTalt;
         rhs = [ NT NTexp; T RDashRArrow; NT NTexp ]; };

       { lhs = NTalt;
         rhs = [ NT NTexp; NT NTgdpat; T RWhere; NT NTdecls ]; };

       { lhs = NTalt;
         rhs = [ NT NTexp; NT NTgdpat ]; };

       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp; NT NTgdpat ]; };

       { lhs = NTgdpat;
         rhs = [ NT NTgd; T RDashRArrow; NT NTexp ]; };

       { lhs = NTstmts;
         rhs = [ NT NTstmtlist; T Semicolon ]; };

       { lhs = NTstmts;
         rhs = [ NT NTstmtlist ]; };

       { lhs = NTstmtlist;
         rhs = [ NT NTstmt; T Semicolon; NT NTstmtlist ]; };

       { lhs = NTstmtlist;
         rhs = [ NT NTstmt ]; };

       { lhs = NTstmt;
         rhs = [ NT NTexp; T Semicolon ]; };

       { lhs = NTstmt;
         rhs = [ NT NTexp; T RLArrowDash; NT NTexp; T Semicolon ]; };

       { lhs = NTstmt;
         rhs = [ T RLet; NT NTdecls; T Semicolon ]; };

       { lhs = NTstmt;
         rhs = [ T Semicolon ]; };

       { lhs = NTfbind;
         rhs = [ NT NTqvar; T REquals; NT NTexp ]; };

       { lhs = NTgcon;
         rhs = [ T LParen; T RParen ]; };

       { lhs = NTgcon;
         rhs = [ T LSquare; T RSquare ]; };

       { lhs = NTgcon;
         rhs = [ T LParen; NT NTcommalist; T RParen ]; };

       { lhs = NTgcon;
         rhs = [ NT NTqcon ]; };

       { lhs = NTqvar;
         rhs = [ T QVarId ]; };

       { lhs = NTqvar;
         rhs = [ T VarId ]; };

       { lhs = NTqvar;
         rhs = [ T LParen; T QVarSym; T RParen ]; };

       { lhs = NTqvar;
         rhs = [ T LParen; T VarSym; T RParen ]; };

       { lhs = NTcon;
         rhs = [ T ConId ]; };

       { lhs = NTcon;
         rhs = [ T LParen; T ConSym; T RParen ]; };

       { lhs = NTqcon;
         rhs = [ T QConId ]; };

       { lhs = NTqcon;
         rhs = [ T ConId ]; };

       { lhs = NTqcon;
         rhs = [ T LParen; T RColon; T RParen ]; };

       { lhs = NTqcon;
         rhs = [ T LParen; T QConSym; T RParen ]; };

       { lhs = NTqcon;
         rhs = [ T LParen; T ConSym; T RParen ]; };

       { lhs = NTqvarop;
         rhs = [ T QVarSym ]; };

       { lhs = NTqvarop;
         rhs = [ T VarSym ]; };

       { lhs = NTqvarop;
         rhs = [ T Backquote; T QVarId; T Backquote ]; };

       { lhs = NTqvarop;
         rhs = [ T Backquote; T VarId; T Backquote ]; };

       { lhs = NTconop;
         rhs = [ T ConSym ]; };

       { lhs = NTconop;
         rhs = [ T Backquote; T ConId; T Backquote ]; };

       { lhs = NTqconop;
         rhs = [ T RColon ]; };

       { lhs = NTqconop;
         rhs = [ T QConSym ]; };

       { lhs = NTqconop;
         rhs = [ T ConSym ]; };

       { lhs = NTqconop;
         rhs = [ T Backquote; T QConId; T Backquote ]; };

       { lhs = NTqconop;
         rhs = [ T Backquote; T ConId; T Backquote ]; };

       { lhs = NTop;
         rhs = [ NT NTqvarop ]; };

       { lhs = NTop;
         rhs = [ NT NTconop ]; };

       { lhs = NTqop;
         rhs = [ NT NTqvarop ]; };

       { lhs = NTqop;
         rhs = [ NT NTqconop ]; };

       { lhs = NTqvarid;
         rhs = [ T QVarId ]; };

       { lhs = NTqvarid;
         rhs = [ T VarId ]; };

       { lhs = NTqconid;
         rhs = [ T QConId ]; };

       { lhs = NTqconid;
         rhs = [ T ConId ]; };

       { lhs = NTliteral;
         rhs = [ T IntLit ]; };

       { lhs = NTliteral;
         rhs = [ T FloatLit ]; };

       { lhs = NTliteral;
         rhs = [ T CharLit ]; };

       { lhs = NTliteral;
         rhs = [ T StringLit ]; };

    |];
};;

let () = begin
  output_tables haskell_cfg "src/computed_actions_gotos.ml"
end
;;
