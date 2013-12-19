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

let lex s =
  let lexemes = Queue.create ()
  and curlexeme = ref { token = NullToken; startpos = -1; endpos = -1 }
  and i = ref 0 in
  let rec newtok tk =
    (* Add any current token to the queue *)
    if !curlexeme.token != NullToken then begin
      curlexeme := {!curlexeme with endpos = !i};
      Queue.add !curlexeme lexemes
    end
    else ();
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
        if !i+1 < String.length s
        then i := !i + 1
        else lex_error "Unmatched '"
        match s.[!i] with
          | '\'' -> lex_error "Empty character literal"
          | '\\' -> match_escape ()
          | _ -> ();
        if !i+1 < String.length s
        then i := !i + 1
        else lex_error "Unmatched '"

    () in
  let rec loop () =
    (* First, check for comments, unless inside string literal. *)
    match !curlexeme.token, s.[!i] with
        | (StringLit, _) -> ()
        | _, '-' when (!i + 1 < String.length s && s.[!i+1] = '-') -> begin
          ()
        end
        | _, '{' when (!i + 1 < String.length s && s.[!i+1] = '-') -> begin
          ()
        end
    (* Process the current character. Note that if the current token continues
     * through this character, we do nothing at all in this matching step. We
     * process fixed-length tokens (char literals and specials) immediately
     * when they start, so curlexeme.token CANNOT be CharLit or Special at this
     * point. *)
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
        | StringLit, '\\' when (!i + 1 < String.length s && s.[!i+1] = '"') ->
            i := !i + 1; () (* Skip over escaped end quote *)
        | _ -> () (* Otherwise, current token continues, so do nothing *);
    i := !i + 1;
    if !i < String.length s then loop () in
  loop ();
  (* Push the last token (if any), and return *)
  match !curlexeme.token with
    | StringLit -> lex_error "Unmatched \""
    | _ -> ()
  newtok NullToken;
  lexemes
;;
