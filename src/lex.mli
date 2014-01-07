open Batteries

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

type pretoken =
  | PreQVarId
  | PreQVarSym
  | PreIntLit
  | PreFloatLit
  | PreCharLit
  | PreStringLit
  | PreSpecial

type lexeme = {
  token     : token;
  contents  : string;
  startraw  : int;    (* Starting index in the raw (unsplit) source string *)
  endraw    : int;
}

type prelexeme = {
  pretoken  : pretoken;
  startix   : int;    (* Starting index in the raw (unsplit) source string *)
  endix     : int;    (* Ending index, points *after* last char. *)
}

(** For debugging purposes. *)
val token_print : 'a BatIO.output -> token -> unit
val lexeme_print : 'a BatIO.output -> lexeme -> unit

(** Compute the indent level of the given character index in the given source
 * string. Uses same line break conventions, and 8 space, aligned tabs. *)
val compute_indent : string -> int -> int

(** Determines whether the given character is the first non-whitespace
 * character on its line. *)
val is_first_non_white : string -> int -> bool

(** Given a raw index into a string, computes the corresponding line number and
 * column number (both starting at 1) in the string. If index is an line
 * breaking character, return the line number before the break, and a column
 * number 1 beyond the last "actual" column. *)
val compute_line_and_col : string -> int -> int * int

(** Tokenize the input program, ignoring all whitespace and using broad
 * categories of tokens. All lexing errors detected here. *)
val prelex : string -> prelexeme Queue.t

(** Converts tokens to their final form, ready for parsing, by further
 * categorizing them (in particular, identifying reserved words and operators)
 * and by copying their contents out of the raw string. *)
val postlex : string -> prelexeme Queue.t -> lexeme Queue.t

(** Use the layout algorithm to insert tokens appropriately for the whitespace
 * in the original program. After this point we no longer need any whitespace
 * information. Input is the token stream before insertion along with original
 * source string (to compute line / indent information) *)
val unlayout : string -> lexeme Queue.t -> lexeme Queue.t
