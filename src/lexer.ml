open Batteries
;;
open Types
;;

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
  | '>' | '?' | '@' | '^' | '|' | '-' | '~' | ':' | '\\' -> true
  | _ -> false
;;
let isspecial = function
  | '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}' -> true
  | _ -> false
;;

(* List of characters is acceptable thing to appear after "--" to make the
 * start of a line comment if it does NOT form a larger symbol, i.e., it has no
 * other symbol characters than '-' at its immediate start. Note that we follow
 * https://ghc.haskell.org/trac/haskell-prime/wiki/LineCommentSyntax
 * and make ':' into a symbol (so --: does not start a comment). This change is
 * in fact incorporated into the errata, see
 * http://www.haskell.org/definition/haskell98-revised-bugs.html *)
let rec has_comment_start = function
  | [] -> true
  | '-'::cs -> has_comment_start cs
  | c::_ -> not (issymb c)
;;

(* Little helper function to compute line break indices in strings. *)
let linebreak_ixs s =
  let rec loop i =
    if i < String.length s then
      if islinebreak s.[i] then
        i :: loop (i+1)
      else
        loop (i+1)
    else
      []
  in loop 0
;;

(* Count the column width of a 1-line string, using 8-space aligned tabs. *)
let countwidth s =
  let do_nextchar i c =
    match c with
    | '\t' -> 8*(i / 8) + 8
    | _ -> i+1
  in String.fold_left do_nextchar 0 s
;;

(* Return the characters on the same line as the given index, but before it. *)
let line_prefix s ix =
  (* Correct for 2-character line breaks. *)
  let s2 = String.nreplace ~str:s ~sub:"\r\n" ~by:"\n" in
  (* Find the last line break before ix, and count from there. *)
  let rec loop lastlb lbs =
    match lbs with
    | n::ns when ix > n -> loop n ns
    | _ -> (String.slice ~first:(lastlb+1) ~last:(ix) s2)
  in loop (-1) (linebreak_ixs s2)
;;

let compute_indent s ix =
  1 + countwidth (line_prefix s ix)
;;

(* This doesn't handle whitespace not made up of whitechars (eg block
 * comments) BUT anyone who uses a block comment inside where the indent
 * whitespace should be deserves what's coming to them. *)
let is_first_non_white s ix =
  List.for_all iswhite (String.explode (line_prefix s ix))

let compute_line_and_col s =
  (* Correct for 2-character line breaks. *)
  let s2 = String.nreplace ~str:s ~sub:"\r\n" ~by:"\n" in
  (* Look for all the line breaks ahead of time. This reflects the way we're
   * actually going to call this function. *)
  let lbixs = linebreak_ixs s2 in
  fun ix ->
    (* Find the last line break before ix, keeping track of how many line
     * breaks we have to look through. *)
    let rec loop line lastlb lbs =
      match lbs with
      | n::ns when ix > n -> loop (line+1) n ns
      | _                 -> (line, ix-lastlb)
    in loop 1 (-1) lbixs
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
      do_nextchar (i+2) cs (InBlockComment (n+1))
    | '-'::'}'::cs, (InBlockComment n) -> begin
      if n = 1 then
        do_nextchar (i+2) cs Default
      else
        do_nextchar (i+2) cs (InBlockComment (n-1))
    end
    | _::cs, (InBlockComment n) -> do_nextchar (i+1) cs (InBlockComment n)
    (* Default cases. *)
    | '{'::'-'::cs, Default -> do_nextchar (i+2) cs (InBlockComment 1)
    | '-'::'-'::cs, Default when has_comment_start cs ->
        do_nextchar (i+2) cs InComment
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
      | c::_, (PreCharLit | PreStringLit) when islinebreak c ->
          raise (Lex_error (i, "Unescaped newline in literal"))
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
    (* Note that we allow qualified reserved ids and operators, contradicting
     * the specification. See
     * https://ghc.haskell.org/trac/haskell-prime/wiki/QualifiedIdentifiers
     * for why the spec is probably a bad idea anyway ('M.wher' 'e' ???). We do
     * NOT alow Haskell 2010 Hierarchical Modules (yet!). *)
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
      | 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' | '"'
      (* escaped ascii control characters *)
      | '\x00' .. '\x1f' -> do_nextchar (i+1) cs (InLexeme plx)
      (* Numeric escapes *)
      | 'o' -> escape_while_pred isoctit (i+1) cs plx
      | 'x' -> escape_while_pred ishexit (i+1) cs plx
      | '0' | '1' .. '9' -> escape_while_pred isdigit (i+1) cs plx
      (* 0 char escape sequence *)
      | '&' ->
          if plx.pretoken = PreStringLit then
            do_nextchar (i+1) cs (InLexeme plx)
          else
            raise (Lex_error (i, "'\\&' in character literal"))
      (* Whitespace gaps *)
      | c when iswhite c ->
          if plx.pretoken = PreStringLit then
            do_escape_gap (i+1) cs plx
          else
            raise (Lex_error (i, "Whitespace gap in character literal"))
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

(* prelexemes to lexemes is 1-1 stream conversion *)
let postlex src prelexemes =
  let lexemes = Queue.create () in
  let do_nextplx plx = begin
    (* Use indices into source string to copy out lexeme contents *)
    let cnts = String.slice ~first:plx.startix ~last:plx.endix src in
    (* Further categorize the token based on lexeme contents *)
    let tkn = begin
      match plx.pretoken with
      | PreSpecial -> begin
        match cnts with
        | "(" -> LParen
        | ")" -> RParen
        | "[" -> LSquare
        | "]" -> RSquare
        | "{" -> LCurly
        | "}" -> RCurly
        | "," -> Comma
        | ";" -> Semicolon
        | "`" -> Backquote
        | _ -> assert false (* Only possible PreSpecial contents *)
      end
      | PreQVarId -> begin
        (* Check if this identifier is reserved *)
        match cnts with
        | "case"     -> RCase
        | "class"    -> RClass
        | "data"     -> RData
        | "default"  -> RDefault
        | "deriving" -> RDeriving
        | "do"       -> RDo
        | "else"     -> RElse
        | "if"       -> RIf
        | "import"   -> RImport
        | "in"       -> RIn
        | "infix"    -> RInfix
        | "infixl"   -> RInfixl
        | "infixr"   -> RInfixr
        | "instance" -> RInstance
        | "let"      -> RLet
        | "module"   -> RModule
        | "newtype"  -> RNewtype
        | "of"       -> ROf
        | "then"     -> RThen
        | "type"     -> RType
        | "where"    -> RWhere
        | "_"        -> RUnderscore
        | _ -> begin
          (* Check if uppercase (varid) or lowercase (conid) after modid *)
          match cnts.[0] with
          | '_' | 'a' .. 'z' -> VarId
          | 'A' .. 'Z' -> begin
            if String.contains cnts '.' then
              match cnts.[1 + String.index cnts '.'] with
              | '_' | 'a' .. 'z' -> QVarId
              | 'A' .. 'Z' -> QConId
              | _ -> assert false (* Only possible starts after modid *)
            else
              ConId
          end
          | _ -> assert false (* Only possible starts of (qualified) ids *)
        end
      end
      | PreQVarSym -> begin
        (* Check if operator reserved *)
        match cnts with
        | ".." -> RDotDot
        | ":"  -> RColon
        | "::" -> RColonColon
        | "="  -> REquals
        | "\\" -> RBackslash
        | "|"  -> RPipe
        | "<-" -> RLArrowDash
        | "->" -> RDashRArrow
        | "@"  -> RAt
        | "~"  -> RTilde
        | "=>" -> REqualsRArrow
        | _ -> begin
          (* Check if starts with colon (consym) or not (varsym) after modid *)
          match cnts.[0] with
          | 'A' .. 'Z' -> begin
            (* Has to be qualified symbol to start w letter *)
            assert (String.contains cnts '.');
            match cnts.[1 + String.index cnts '.'] with
            | ':' -> QConSym
            | _ -> QVarSym
          end
          | ':' -> ConSym
          | _ -> VarSym
        end
      end
      | PreIntLit    -> IntLit
      | PreFloatLit  -> FloatLit
      | PreCharLit   -> CharLit
      | PreStringLit -> StringLit
    end in
    Queue.add { token = tkn;
                Types.contents = cnts;
                startraw = plx.startix;
                endraw = plx.endix } lexemes
  end
  in begin
    Queue.iter do_nextplx prelexemes;
    lexemes
  end
;;

type lexeme_or_indent =
  | SomeLexeme of lexeme
  | IndentBlock of int (* {n} in specification *)
  | IndentLine of int (* <n> in specification *)
let unlayout src_string lexemes_orig =
  let lexemes = Queue.copy lexemes_orig (* Don't modify input argument *)
  and inter_lxs = Queue.create () (* Intermediate queue for layout algo *)
  and final_lexemes = Queue.create ()
  and lastln = ref 0
  in begin
    Util.dbg "Adding implicit layout tokens for lexeme queue %a\n"
      (Queue.print ~first:"\n[ " ~last:" ]\n" ~sep:"\n  " Print.lexeme_print)
      lexemes_orig;
    (* Rule #2: Add IndentBlock at start of file *)
    if (not (Queue.is_empty lexemes)) &&
       (Queue.peek lexemes).token <> LCurly &&
       (Queue.peek lexemes).token <> RModule then begin
      Queue.add (IndentBlock (compute_indent src_string
        (Queue.peek lexemes).startraw)) inter_lxs;
      (* Process first lexeme immediately for special case of Rule #3 *)
      let firstlx = Queue.take lexemes in
      lastln := fst (compute_line_and_col src_string firstlx.startraw);
      Queue.add (SomeLexeme firstlx) inter_lxs
    end;
    (* Walk through the remaining lexemes, applying rules #1 and #3 *)
    while not (Queue.is_empty lexemes) do
      let lx = Queue.take lexemes in
      match lx.token with
      (* Rule #1: Add IndentBlock after appropriate keywords *)
      | RLet
      | RWhere
      | RDo
      | ROf -> begin
        if Queue.is_empty lexemes ||
           (Queue.peek lexemes).token <> LCurly then begin
            Queue.add (SomeLexeme lx) inter_lxs;
            let nextlx = Queue.take lexemes
            in begin
              Queue.add (IndentBlock (compute_indent src_string
                  nextlx.startraw)) inter_lxs;
              (* Process next lexeme immediately for special case of Rule #3 *)
              lastln := fst (compute_line_and_col src_string nextlx.startraw);
              Queue.add (SomeLexeme nextlx) inter_lxs;
            end
        end
      end
      | _ -> ();
      (* Rule #3: Add IndentLine before 1st token on line *)
      if is_first_non_white src_string lx.startraw then
        Queue.add (IndentLine (compute_indent src_string lx.startraw))
          inter_lxs;
      Queue.add (SomeLexeme lx) inter_lxs
    done;
    Util.dbg "Input to transformation L: %a\n"
      (Queue.print ~first:"\n[ " ~last:" ]\n" ~sep:"\n  "
        (fun o ilx ->
          match ilx with
          | IndentBlock n -> Printf.fprintf o "IndentBlock %d" n
          | IndentLine n -> Printf.fprintf o "IndentLine %d" n
          | SomeLexeme lx -> Print.lexeme_print o lx))
      inter_lxs;
    (* inter_lxs contains input to translation L (in Report sec 9.3). Now apply
     * translation L, yielding final queue to return. *)
    (* Helper functions *)
    let add_implicit_L () =
      Queue.add { token = LCurly;
                  Types.contents = "{";
                  startraw = -1;
                  endraw = -1; } final_lexemes
    and add_implicit_R () =
      Queue.add { token = RCurly;
                  Types.contents = "}";
                  startraw = -1;
                  endraw = -1; } final_lexemes
    and add_implicit_semi () =
      Queue.add { token = Semicolon;
                  Types.contents = ";";
                  startraw = -1;
                  endraw = -1; } final_lexemes
    (* Loop through input queue, passing along layout context. Straight-up
     * implementation of patterns for transformation L. *)
    in
    let rec loop layout_ctx = begin
      Util.dbg "Looped on context %a\n" print_guess layout_ctx;
      if Queue.is_empty inter_lxs then
        match layout_ctx with
        | [] -> ()
        | m::ms -> begin
          if m = 0 then
            raise (Lex_error (-1, "Unmatched {")) (* Note 6 *)
          else
            add_implicit_R ();
          loop ms
        end
      else
        let ilx = Queue.take inter_lxs in
        match (ilx,layout_ctx) with
        | (IndentLine n), ms -> do_indentline n ms
        (* Start of new layout block *)
        | (IndentBlock n), m::ms when n > m -> begin
          add_implicit_L ();
          loop (n::(m::ms));
        end
        (* Start of new (top level) layout block *)
        | (IndentBlock n), [] when n > 0 -> begin
          add_implicit_L ();
          loop (n::[])
        end
        (* Note 1 contradicts Note 2, but Note 2 matches the explanatory text
         * in Report section 2.7, so let's go with that. *)
        (* Note 2 *)
        | (IndentBlock n), ms -> begin
          add_implicit_L ();
          add_implicit_R ();
          do_indentline n ms
        end
        | (SomeLexeme lx), mss ->
            match (lx.token,mss) with
            (* { starts explicit (0 width) layout context *)
            | LCurly, ms -> begin
              Queue.add lx final_lexemes;
              loop (0::ms)
            end
            (* } ends explicit (0 width) layout context *)
            | RCurly, 0::ms -> begin
              Queue.add lx final_lexemes;
              loop ms
            end
            (* Note 3 *)
            | RCurly, _ -> raise (Lex_error (lx.startraw, "Unmatched }"))
            (* Otherwise, just move the lexeme along. *)
            (* Note that we IGNORE Note 5, because it will tangle our parser
             * and lexer up horribly. So implicit closing braces are only
             * added when indent level decreases. The only serious change this
             * makes to the language is it prevents us from putting "in" on
             * the same line as "let" conditions, eg
             * > let x = 1 in
             * >   foo x
             * will be converted to
             * > let {x = 1 in
             * >   }foo x
             * and will not parse. To avoid this, write
             * > let x = 1
             * > in
             * >   foo x
             * or
             * > let x = 1
             * > in foo x
             * All other clauses (where, do, of) do not have this problem as
             * in standard Haskell style they're ended by a line break, not a
             * keyword, so this shouldn't be an issue.
             * (Yes, these alternatives are ugly, but seriously? Fall back to
             * inserting tokens on a PARSE ERROR? Are you nuts?) *)
            | _, ms -> begin
              Queue.add lx final_lexemes;
              loop ms
            end
    end
    (* Stupid helper function, to deal with some messy recursion (for when the
     * same IndentLine or bare block ends multiple layout contexts. *)
    and do_indentline n mss =
      (* Indent level stays the same, current block continues. *)
      match mss with
      | m::_ when n = m -> begin
        add_implicit_semi ();
        loop mss
      end
      (* Indent level decreases, end of layout block. Loop back on same token
       * in case multiple layout blocks end here. *)
      | m::ms when n < m -> begin
        add_implicit_R ();
        do_indentline n ms
      end
      (* Indent level increases (but not at start of layout block), so
       * this is line continuation, just keep going. Note that since this can
       * appear inside recursive call to do_indent, we can in fact have a
       * continuation that contains a block in it (YUCK!) *)
      | _ -> loop mss
    in loop [];
    (* Add EOF token to keep the parser happy. *)
    Queue.add { token = EOF; Types.contents = ""; startraw = -1;
                endraw = -1 } final_lexemes;
    final_lexemes
  end
;;
