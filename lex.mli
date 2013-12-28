type token =
  | QVarId
  | QConId
  | QVarSym
  | QConSym
  | IntLit
  | FloatLit
  | CharLit
  | StringLit
  | Special
  | NullToken     (* These 3 are the "fake" tokens used for lexing that will
                     not be added to the token queue *)
  | Comment
  | BlockComment

type lexeme = {
  token     : token;  (* token type of the lexeme *)
  startline : int;    (* Line of the source string the lexeme starts on *)
  endline   : int;    (* and ends on. Only strings can be multi line *)
  startpos  : int;    (* in source string, starting position ... *)
  endpos    : int;    (* ... and ending position of the lexeme. Following
                           C conventions endpos is after last char *)
}

type lexeme_stream

val lex : string -> lexeme_stream
