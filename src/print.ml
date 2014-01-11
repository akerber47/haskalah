open Batteries
;;
open Types
;;

let token_print o t =
  let s =
    match t with
    | EOF -> "EOF"
    | VarId -> "VarId"
    | ConId -> "ConId"
    | VarSym -> "VarSym"
    | ConSym -> "ConSym"
    | QVarId -> "QVarId"
    | QConId -> "QConId"
    | QVarSym -> "QVarSym"
    | QConSym -> "QConSym"
    | IntLit -> "IntLit"
    | FloatLit -> "FloatLit"
    | CharLit -> "CharLit"
    | StringLit -> "StringLit"
    | RCase -> "RCase"
    | RClass -> "RClass"
    | RData -> "RData"
    | RDefault -> "RDefault"
    | RDeriving -> "RDeriving"
    | RDo -> "RDo"
    | RElse -> "RElse"
    | RIf -> "RIf"
    | RImport -> "RImport"
    | RIn -> "RIn"
    | RInfix -> "RInfix"
    | RInfixl -> "RInfixl"
    | RInfixr -> "RInfixr"
    | RInstance -> "RInstance"
    | RLet -> "RLet"
    | RModule -> "RModule"
    | RNewtype -> "RNewtype"
    | ROf -> "ROf"
    | RThen -> "RThen"
    | RType -> "RType"
    | RWhere -> "RWhere"
    | RUnderscore -> "RUnderscore"
    | RDotDot -> "RDotDot"
    | RColon -> "RColon"
    | RColonColon -> "RColonColon"
    | REquals -> "REquals"
    | RBackslash -> "RBackslash"
    | RPipe -> "RPipe"
    | RLArrowDash -> "RLArrowDash"
    | RDashRArrow -> "RDashRArrow"
    | RAt -> "RAt"
    | RTilde -> "RTilde"
    | REqualsRArrow -> "REqualsRArrow"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LSquare -> "LSquare"
    | RSquare -> "RSquare"
    | LCurly -> "LCurly"
    | RCurly -> "RCurly"
    | Comma -> "Comma"
    | Semicolon -> "Semicolon"
    | Backquote -> "Backquote"
  in Printf.fprintf o "%s" s
;;

let lexeme_print o l =
  Printf.fprintf o "(%a \"%s\" src[%d:%d])"
    token_print l.token l.Types.contents l.startraw l.endraw
;;

let nonterm_print o nt =
  let s =
    match nt with
    | Goal -> "Goal"
    | NTmodule -> "NTmodule"
    | NTbody -> "NTbody"
    | NTimpdecls -> "NTimpdecls"
    | NTexports -> "NTexports"
    | NTexport -> "NTexport"
    | NTimpdecl -> "NTimpdecl"
    | NTimpspec -> "NTimpspec"
    | NTimport -> "NTimport"
    | NTqcname -> "NTqcname"
    | NTtopdecls -> "NTtopdecls"
    | NTtopdecl -> "NTtopdecl"
    | NTdecls -> "NTdecls"
    | NTdecl -> "NTdecl"
    | NTcdecls -> "NTcdecls"
    | NTcdecl -> "NTcdecl"
    | NTidecls -> "NTidecls"
    | NTidecl -> "NTidecl"
    | NTgendecl -> "NTgendecl"
    | NTops -> "NTops"
    | NTqvars -> "NTqvars"
    | NTfixity -> "NTfixity"
    | NTtype -> "NTtype"
    | NTbtype -> "NTbtype"
    | NTatype -> "NTatype"
    | NTgtycon -> "NTgtycon"
    | NTcontext -> "NTcontext"
    | NTclass -> "NTclass"
    | NTscontext -> "NTscontext"
    | NTsimpleclass -> "NTsimpleclass"
    | NTsimpletype -> "NTsimpletype"
    | NTconstrs -> "NTconstrs"
    | NTconstr -> "NTconstr"
    | NTnewconstr -> "NTnewconstr"
    | NTfielddecl -> "NTfielddecl"
    | NTderiving -> "NTderiving"
    | NTdclass -> "NTdclass"
    | NTinst -> "NTinst"
    | NTrhs -> "NTrhs"
    | NTgdrhs -> "NTgdrhs"
    | NTgd -> "NTgd"
    | NTexp -> "NTexp"
    | NTinfixexp -> "NTinfixexp"
    | NTexp10 -> "NTexp10"
    | NTaexp -> "NTaexp"
    | NTqual -> "NTqual"
    | NTalts -> "NTalts"
    | NTalt -> "NTalt"
    | NTgdpat -> "NTgdpat"
    | NTstmts -> "NTstmts"
    | NTstmt -> "NTstmt"
    | NTfbind -> "NTfbind"
    | NTgcon -> "NTgcon"
    | NTqvar -> "NTqvar"
    | NTcon -> "NTcon"
    | NTqcon -> "NTqcon"
    | NTqvarop -> "NTqvarop"
    | NTconop -> "NTconop"
    | NTqconop -> "NTqconop"
    | NTop -> "NTop"
    | NTqop -> "NTqop"
    | NTgconsym -> "NTgconsym"
    | NTimpdecllist -> "NTimpdecllist"
    | NTexportlist -> "NTexportlist"
    | NTqcnamelist -> "NTqcnamelist"
    | NTimportlist -> "NTimportlist"
    | NTtopdecllist -> "NTtopdecllist"
    | NTqvarlist -> "NTqvarlist"
    | NTtypelist -> "NTtypelist"
    | NTcommalist -> "NTcommalist"
    | NToplist -> "NToplist"
    | NTclasslist -> "NTclasslist"
    | NTsimpleclasslist -> "NTsimpleclasslist"
    | NTconstrlist -> "NTconstrlist"
    | NTfielddecllist -> "NTfielddecllist"
    | NTdclasslist -> "NTdclasslist"
    | NTexplist -> "NTexplist"
    | NTquallist -> "NTquallist"
    | NTfbindlist -> "NTfbindlist"
    | NTdecllist -> "NTdecllist"
    | NTcdecllist -> "NTcdecllist"
    | NTidecllist -> "NTidecllist"
    | NTatypelist -> "NTatypelist"
    | NTtyvarlist -> "NTtyvarlist"
    | NTtyvarcommalist -> "NTtyvarcommalist"
    | NTaexplist -> "NTaexplist"
    | NTaltlist -> "NTaltlist"
    | NTstmtlist -> "NTstmtlist"
    | NTqvarid -> "NTqvarid"
    | NTqconid -> "NTqconid"
    | NTliteral -> "NTliteral"
  in Printf.fprintf o "%s" s
;;

let ast0_print o _ =
  Printf.fprintf o "TODO" (* TODO *)
;;
