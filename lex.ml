(* Takes in a string (a program) and spits out a list of lexemes.
 * See http://www.haskell.org/onlinereport/lexemes.html
 *)

type token =
  | NullToken
  | VarId
  | ConId
  | VarSym
  | IntLit
  | CharLit
  | StringLit
  | Special;;

type lexeme =
  { token     : token;  (* token type of the lexeme *)
    startline : int;    (* Line of the source string the lexeme starts on *)
    endline   : int;    (* and ends on. Only strings can be multi line *)
    startpos  : int;    (* in source string, starting position ... *)
    endpos    : int;    (* ... and ending position of the lexeme. Following
                           standard C conventions endpos is after last char *)
  };;

let iswhite = function
  | '\r' | '\n' | '\x0b' | '\x0c' | ' ' | '\t' -> true
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

let lex program =
  let lexemes = Queue.create ()
  and curlexeme = ref { token = NullToken; startline = 1; endline = -1;
                        startpos = -1; endpos = -1 }
  and i = ref 0 in
  (* Take the current token, finish it, and add it to the queue. Start a new
   * token of the given type (identified by its 1st character). If the token
   * has fixed (known) length, slurp up the rest of it immediately. *)
  let rec newtok tk =
    (* Add any current token to the queue *)
    if !curlexeme.token != NullToken then begin
      curlexeme := {!curlexeme with endpos = !i};
      Queue.add !curlexeme lexemes
    end;
    curlexeme := { token = tk; startpos = !i; endpos = -1 };
    (* Check for token types we can process immediately *)
    match tk with
      (* Special tokens are only 1 character long *)
      | Special -> begin
        i := !i + 1;
        newtok NullToken
      end
      (* Process character literals immediately *)
      | CharLit -> begin
        if !i+1 < String.length s then
          i := !i + 1
        else
          lex_error "Unmatched '";
        match s.[!i] with
          | '\'' -> lex_error "Empty character literal"
          (* TODO handle '\&' error case (0 width char literal). Maybe deal
           * with this when parsing? *)
          | '\\' -> match_escape ()
          | _ -> ();
        if !i+1 < String.length s then
          i := !i + 1
        else
          lex_error "Unmatched '";
        if s.[!i] != '\'' then
          lex_error "Character literal too long";
        i := !i + 1;
        newtok NullToken
      | _ -> ();
    ()
  and nextchar () =
    if !i < String.length s then begin
      match !curlexeme.token, s.[!i] with
        (* First, check for comments, unless inside string literal. *)
        | _, '-' when (!curlexeme.token != StringLit &&
                       !i + 1 < String.length s && s.[!i+1] = '-') -> begin
          (* End of line comment: look for \r, \n, or \r\n *)
        end
        | _, '{' when (!curlexeme.token != StringLit &&
                       !i + 1 < String.length s && s.[!i+1] = '-') -> begin
          ()
        end
        | _, _ -> ();
      (* Process the current character. Note that if the current token
       * continues through this character, we do nothing at all in this
       * matching step. We process fixed-length tokens (char literals and
       * specials) immediately when they start, so curlexeme.token CANNOT be
       * CharLit or Special at this point. *)
      match !curlexeme.token, s.[!i] with
        (* Whitespace or special chars ends the current token,
         * except in string literals *)
        | StringLit, c when iswhite(c) || isspecial(c) -> ()
        | _, c when iswhite(c) -> newtok NullToken
        | _, c when isspecial(c) -> newtok Special
        (* Note that digits can appear within varids / conids. Other than that
         * these arbitrary-length tokens basically end / start whenever the
         * character class changes. *)
        | NullToken | VarSym | IntLit, '_' | 'a' .. 'z' ->
            newtok VarId
        | NullToken | VarSym | IntLit, 'A' .. 'Z' ->
            newtok ConId
        | NullToken | VarId | ConId | IntLit, c when issymb(c) ->
            newtok VarSym
        | NullToken | VarSym , '0' | '1' .. '9' ->
            newtok IntLit
        | NullToken | VarId | ConId | VarSym | IntLit, '\'' ->
            newtok CharLit
        | NullToken | VarId | ConId | VarSym | IntLit, '"' ->
            newtok StringLit
        (* String literals: watch out for escaping and end quotes *)
        | StringLit, '"' -> newtok NullToken
        | StringLit, '\\' -> match_escape ()
        (* Otherwise, current token continues, so do nothing *)
        | _ -> ();
      i := !i + 1;
      nextchar ()
    end in
  nextchar ();
  (* Push the last token (if any), and return *)
  if !curlexeme.token = StringLit then
    lex_error "Unmatched \"";
  newtok NullToken;
  lexemes
;;
