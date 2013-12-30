open Batteries

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
  (* TODO, many more token types needed for parsing. *)

type pretoken =
  | PreQVarId
  | PreQConId
  | PreQVarSym
  | PreQConSym
  | PreIntLit
  | PreFloatLit
  | PreCharLit
  | PreStringLit
  | PreSpecial

type lexeme = {
  token     : token;
  contents  : string;
  (* Line and column for error reporting purposes. Tokens added in the
   * layout-handling step have column -1 (but the appropriate line #). *)
  startline : int;    (* Line of the source string the lexeme starts on (and
                         ends on, unless it's a multiline string literal) *)
  startcol  : int;    (* Column ... *)
}

type prelexeme = {
  pretoken  : pretoken;
  startix   : int;    (* Starting index in the raw (unsplit) source string *)
  endix     : int;    (* Ending index, points *after* last char. *)
}

(** Split the given input program on line breaks, and compute the indent level
 *  of each line. Uses \r\n if found, \r or \n otherwise. *)
val compute_indents : string -> int Array.t

(** Given a raw index into a string, computes the corresponding line number and
 * column number (both starting at 1) in the string. If index is an line
 * breaking character, return the line number before the break, and a column
 * number 1 beyond the last "actual" column. *)
val compute_line_and_col : string -> int -> int * int

(** Tokenize the input program, ignoring all whitespace and using broad
 * categories of tokens. *)
val prelex : string -> prelexeme Queue.t

(** Converts tokens to their final form, ready for parsing, by further
 * categorizing them and by copying their contents out of the raw string. *)
val postlex : string -> prelexeme Queue.t -> lexeme Queue.t

(** Use the layout algorithm to insert tokens appropriately for the whitespace
 * in the original program. After this point we no longer need any whitespace
 * information. Input is the token stream before insertion, and the indent
 * level of each line of the source program. *)
val unlayout : lexeme Queue.t -> int Array.t -> lexeme Queue.t
