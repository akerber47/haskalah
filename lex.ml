open Batteries
;;

(* --- BEGIN DUPLICATED TYPES from mli --- *)
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
  | PreQVarSym
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

(* --- END DUPLICATED TYPES from mli --- *)


let islinebreak = function
  | '\r' | '\n' | '\x0c' -> true
  | _ -> false
;;

let iswhite = function
  | '\r' | '\n' | '\x0b' | '\x0c' | ' ' | '\t' -> true
  | _ -> false
;;

let isdigit = function
  | '0' | '1' .. '9' -> true
  | _ -> false
;;

let isoctit = function
  | '0' | '1' .. '7' -> true
  | _ -> false
;;

let ishexit = function
  | '0' | '1' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let islower = function
  | '_' | 'a' .. 'z' -> true
  | _ -> false
;;

let isupper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let isnamechar = function
  | '_' | '0' | '1' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '\'' -> true
  | _ -> false
;;

let issymb = function
  | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' -> true
  | '?' | '@' | '^' | '|' | '-' | '~' | ':' | '\\' -> true
  | _ -> false
;;
let isspecial = function
  | '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}' -> true
  | _ -> false
;;

(* List of characters is acceptable thing to appear after "--" to make the start
 * of a line comment if it does NOT form a larger symbol, i.e., it has
 * no other symbol characters than '-' at its immediate start. *)
let rec has_comment_start = function
  | [] -> true
  | '-'::cs -> has_comment_start cs
  | c::_ -> not (issymb c)
;;

let compute_indents s =
  (* Note that we slightly violate the specification, in that technically the
   * indent should be the indented position of the first lexeme, but if the
   * line contains a block comment before the first lexeme we return the
   * starting position of the block comment.
   * (We do the same for line comments, but it's irrelevant since if we hit
   * a line comment there cannot be any lexemes on this line anyways)
   * But this way makes more sense anyways, for anyone actually reading the
   * code. Screw it. *)
  let compute_indent line =
    let rec loop i ind =
      if i < String.length line then
        match line.[i] with
        (* Vertical tabs count as 1 white char *)
        | '\x0b'
        | ' '  -> loop (i+1) (ind+1)
        (* Tabs align to next tap stop (multiples of 8 spaces) *)
        | '\t' -> loop (i+1) (8*(ind / 8)+8)
        | _    -> ind
      else
        ind
    in loop 0 0
  and lines = Str.split (Str.regexp "\\(\r\n\\)\\|[\r\n\x0c]") s
  in Array.of_list (List.map compute_indent lines)
;;

let compute_line_and_col s =
  (* Correct for 2-character line breaks. *)
  let s2 = String.nreplace s "\r\n" "\n" in
  (* Look for all the line breaks ahead of time. This reflects the way we're
   * actually going to call this function. *)
  let linebreak_ixs =
    let rec loop i =
      if i < String.length s2 then
        if islinebreak s2.[i] then
          i :: loop (i+1)
        else
          loop (i+1)
      else
        []
    in loop 0
  in fun ix ->
    let rec loop line lastlb lbixs =
      match lbixs with
      | n::ns when ix > n -> loop (line+1) n ns
      | _                 -> (line, ix-lastlb)
    in loop 1 (-1) linebreak_ixs
;;

(* Current internal state of the lexer *)
type lex_state =
  | Default
  | InComment
  | InBlockComment of int (* Depth of nested block comment *)
  | InLexeme of prelexeme
  (* These are our few extra "DFA-ish" states which we need to keep track of
   * some extra stuff about the current lexeme. We store the starting index
   * of the current lexeme we're partway through. *)
  | InModId of int (* Inside a qualified name, but don't know of what sort. *)
  | InOctInt of int (* Inside octal int literal *)
  | InHexInt of int (* Inside hexadecimal int literal *)
  | InFloatExp of int (* Inside a float literal, and have eaten the e|E. *)
;;

(* Lex error: index in input string where it occurred, and error message. *)
exception Lex_error of int * string
;;

let prelex src_string =
  let prelexemes = Queue.create ()
  and src_chars = String.to_list src_string in
  (* do_nextchar : i -> char list -> state -> prelexeme Queue.t.
   * Takes in list of input characters to pull from, a current state, and a
   * counter (of current position in overall input).
   * Processes the top character (and maybe the next few). Is
   * very, very tail recursive. *)
  let rec do_nextchar i clst state = match (clst,state) with
    (* End of file cases. Check for some errors, and end any current token. *)
    | [], Default
    | [], InComment -> prelexemes
    | [], (InBlockComment _) -> raise (Lex_error (i, "Unmatched {-"))
    | [], (InLexeme plx) -> begin
      match plx.pretoken with
      | PreCharLit -> raise (Lex_error (i,"Unmatched '"))
      | PreStringLit -> raise (Lex_error (i,"Unmatched \""))
      | _ -> endlexeme {plx with endix = i}; prelexemes
    end
    (* Line comment cases. End on line breaks. *)
    | c::cs, InComment when islinebreak(c) -> do_nextchar (i+1) cs Default
    | _::cs, InComment -> do_nextchar (i+1) cs InComment
    (* Block comment cases. Get deeper on {-, less deep on -} *)
    | '{'::'-'::cs, (InBlockComment n) ->
      do_nextchar (i+1) cs (InBlockComment (n+1))
    | '-'::'}'::cs, (InBlockComment n) -> begin
      if n = 1 then
        do_nextchar (i+1) cs Default
      else
        do_nextchar (i+1) cs (InBlockComment (n-1))
    end
    | _::cs, (InBlockComment n) -> do_nextchar (i+1) cs (InBlockComment n)
    (* Default cases. *)
    | '{'::'-'::cs, Default -> do_nextchar (i+1) cs (InBlockComment 1)
    | '-'::'-'::cs, Default when has_comment_start cs ->
        do_nextchar (i+1) cs InComment
    (* Whitespace is boring. *)
    | c::cs, Default when iswhite c -> do_nextchar (i+1) cs Default
    (* Special chars are only 1 char long, so end them immediately. *)
    | c::cs, Default when isspecial c -> begin
      endlexeme { pretoken = PreSpecial; startix = i; endix = i+1; };
      do_nextchar (i+1) cs Default
    end
    (* Start of various special things *)
    | c::cs, Default when isupper c -> do_nextchar (i+1) cs (InModId i)
    | '0'::(('o'|'O')::(c::cs)), Default when isoctit c ->
        do_nextchar (i+3) cs (InOctInt i)
    | '0'::(('x'|'X')::(c::cs)), Default when ishexit c ->
        do_nextchar (i+3) cs (InHexInt i)
    (* Starts of various tokens. *)
    | c::cs, Default when issymb c -> do_nextchar (i+1) cs (InLexeme
        { pretoken = PreQVarSym; startix = i; endix = -1; })
    | c::cs, Default when islower c -> do_nextchar (i+1) cs (InLexeme
        { pretoken = PreQVarId; startix = i; endix = -1; })
    | c::cs, Default when isdigit c -> do_nextchar (i+1) cs (InLexeme
        { pretoken = PreIntLit; startix = i; endix = -1; })
    | '\''::cs, Default -> do_nextchar (i+1) cs (InLexeme
        { pretoken = PreCharLit; startix = i; endix = -1; })
    | '"'::cs, Default -> do_nextchar (i+1) cs (InLexeme
        { pretoken = PreStringLit; startix = i; endix = -1; })
    (* This error case will also be reached if we're in the middle of a token /
     * special state, because we'll first end the current token / state then
     * call do_nextchar again on that char with default state. *)
    | _, Default -> raise (Lex_error (i, "Invalid character"))
    (* This is where things get complicated. *)
    | css, (InLexeme plx) -> begin
      match css,plx.pretoken with
      (* Escape sequences and end characters in chars/strings *)
      | '\\'::cs, (PreCharLit | PreStringLit) -> do_escape (i+1) cs plx
      | c::_, PreCharLit when islinebreak c ->
          raise (Lex_error (i, "Unmatched '"))
      | '\''::cs, PreCharLit -> begin
        endlexeme {plx with endix = i+1};
        do_nextchar (i+1) cs Default
      end
      | '"'::cs, PreStringLit -> begin
        endlexeme {plx with endix = i+1};
        do_nextchar (i+1) cs Default
      end
      (* Dot in int literal, followed by digit -> float literal *)
      | '.'::(c::cs), PreIntLit when isdigit c ->
          do_nextchar (i+2) cs (InLexeme {plx with pretoken = PreFloatLit})
      (* e|E in int/float lit [opt w sign], followed by digit -> floatexp *)
      | ('e'|'E')::(c::cs), (PreIntLit | PreFloatLit) when isdigit c ->
          do_nextchar (i+2) cs (InFloatExp plx.startix)
      | ('e'|'E')::(('+' | '-')::(c::cs)), (PreIntLit | PreFloatLit)
          when isdigit c ->
          do_nextchar (i+3) cs (InFloatExp plx.startix)
      (* In these cases, the current token continues. *)
      | _::cs, (PreCharLit | PreStringLit) ->
          do_nextchar (i+1) cs (InLexeme plx)
      | c::cs, (PreIntLit | PreFloatLit) when isdigit c ->
          do_nextchar (i+1) cs (InLexeme plx)
      | c::cs, PreQVarId when isnamechar c ->
          do_nextchar (i+1) cs (InLexeme plx)
      | c::cs, PreQVarSym when issymb c ->
          do_nextchar (i+1) cs (InLexeme plx)
      (* In any other case, the current lexeme ends. Rather than actually
       * processing the next character, just end the lexeme and go back to
       * the top for this character. *)
      | _, _ -> begin
        endlexeme {plx with endix = i};
        do_nextchar i css Default
      end
    end
    (* Finally, the weird special cases. *)
    (* Continuation and end of oct/hex literals *)
    | c::cs, (InOctInt n) when isoctit c ->
        do_nextchar (i+1) cs (InOctInt n)
    | c::cs, (InHexInt n) when ishexit c ->
        do_nextchar (i+1) cs (InHexInt n)
    | css, (InOctInt n)
    | css, (InHexInt n) -> begin
      endlexeme { pretoken = PreIntLit; startix = n; endix = i };
      do_nextchar i css Default
    end
    (* Continuation and end of floatexps *)
    | c::cs, (InFloatExp n) when isdigit c ->
        do_nextchar (i+1) cs (InFloatExp n)
    | css, (InFloatExp n) -> begin
      endlexeme { pretoken = PreFloatLit; startix = n; endix = i };
      do_nextchar i css Default
    end
    (* Continuation and end / conversion of modids *)
    | c::cs, (InModId n) when isnamechar c ->
        do_nextchar (i+1) cs (InModId n)
    | '.'::(c::cs), (InModId n) when issymb c ->
        do_nextchar (i+2) cs (InLexeme
        { pretoken = PreQVarSym; startix = n; endix = -1 })
    | '.'::(c::cs), (InModId n) when islower c || isupper c ->
        do_nextchar (i+2) cs (InLexeme
        { pretoken = PreQVarId; startix = n; endix = -1 })
    (* ModId which doesn't qualify a name is really a PreQVarId *)
    | css, (InModId n) -> begin
      endlexeme { pretoken = PreQVarId; startix = n; endix = i };
      do_nextchar i css Default
    end
  (* do_escape : int -> char list -> prelexeme -> prelexeme Queue.t
   * Calls to/from do_nextchar mutually recursively. Processes escape sequences
   * in character / string literals. The given char list starts with the first
   * character *after* the backslash that begins the escape sequence. *)
  and do_escape i css plx =
    let rec do_escape_gap i css plx =
      match css with
      | [] -> raise (Lex_error (i, "Unmatched \""))
      | '\\'::cs -> do_nextchar (i+1) cs (InLexeme plx)
      | c::cs when iswhite c -> do_escape_gap (i+1) cs plx
      | _ -> raise (Lex_error (i, "Non-whitespace character in gap"))
    and escape_while_pred pred i css plx =
      match css with
      | [] -> raise (Lex_error (i, "Unmatched \""))
      | c::cs when pred c -> escape_while_pred pred (i+1) cs plx
      | css -> do_nextchar i css (InLexeme plx)
    in
    match css with
    | [] -> raise (Lex_error (i, "Unterminated escape sequence"))
    | c::cs -> begin
      match c with
      (* 1 character escape sequences *)
      | 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' | '"' | '&'
      (* escaped ascii control characters *)
      | '\x00' .. '\x1f' -> do_nextchar (i+1) cs (InLexeme plx)
      (* Numeric escapes *)
      | 'o' -> escape_while_pred isoctit (i+1) cs plx
      | 'x' -> escape_while_pred ishexit (i+1) cs plx
      | '0' | '1' .. '9' -> escape_while_pred isdigit (i+1) cs plx
      (* Whitespace gaps *)
      | c when iswhite c -> do_escape_gap (i+1) cs plx
      | _ -> begin
        (* Literal ASCII escapes *)
        match css with
        (* 3 letters *)
        | 'N'::('U'::('L'::cs))
        | 'S'::('O'::('H'::cs))
        | 'S'::('T'::('X'::cs))
        | 'E'::('T'::('X'::cs))
        | 'E'::('O'::('T'::cs))
        | 'E'::('N'::('Q'::cs))
        | 'A'::('C'::('K'::cs))
        | 'B'::('E'::('L'::cs))
        | 'D'::('L'::('E'::cs))
        | 'D'::('C'::('1'::cs))
        | 'D'::('C'::('2'::cs))
        | 'D'::('C'::('3'::cs))
        | 'D'::('C'::('4'::cs))
        | 'N'::('A'::('K'::cs))
        | 'S'::('Y'::('N'::cs))
        | 'E'::('T'::('B'::cs))
        | 'C'::('A'::('N'::cs))
        | 'S'::('U'::('B'::cs))
        | 'E'::('S'::('C'::cs))
        | 'D'::('E'::('L'::cs)) ->
            do_nextchar (i+3) cs (InLexeme plx)
        (* 2 letters *)
        | 'B'::('S'::cs)
        | 'H'::('T'::cs)
        | 'L'::('F'::cs)
        | 'V'::('T'::cs)
        | 'F'::('F'::cs)
        | 'C'::('R'::cs)
        | 'S'::('O'::cs)
        | 'S'::('I'::cs)
        | 'F'::('S'::cs)
        | 'G'::('S'::cs)
        | 'R'::('S'::cs)
        | 'U'::('S'::cs)
        | 'S'::('P'::cs) ->
            do_nextchar (i+2) cs (InLexeme plx)
        | _ -> raise (Lex_error (i, "Invalid escape sequence"))
      end
    end
  (* To end lexeme, just add it to the queue. *)
  and endlexeme plx = Queue.add plx prelexemes
  in
  do_nextchar 0 src_chars Default
;;

let postlex s q = Queue.create ()
;;

let unlayout q a = Queue.create ()
;;
