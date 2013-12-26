(* Takes in a string (a program) and spits out a list of lexemes.
 * See http://www.haskell.org/onlinereport/lexemes.html
 *)

type token =
  | VarId
  | ConId
  | VarSym
  | IntLit
  | CharLit
  | StringLit
  | Special
  | NullToken     (* These 3 are the "fake" tokens used for lexing that will
                     not be added to the token queue *)
  | Comment
  | BlockComment;;

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

(* Something that looks like a VarSym is actually a start of 1-line comment
 * if it consists of 2 or more copies of the '-' character. *)
let is_comment_start s =
  String.length s >= 2 && s = String.make (String.length s) '-'
;;

let match_forward re s =
  try ignore (Str.search_forward re s 0); true
  with Not_found -> false
;;

let lex program =
  let lexemes = Queue.create ()
  and curlexeme = ref { token = NullToken; startline = 1; endline = -1;
                        startpos = -1; endpos = -1 }
  and lines =
    if match_forward (Str.regexp "\r\n") program then
      Str.split (Str.regexp "\\(\r\n\\)\\|\x0c") program
    else
      Str.split (Str.regexp "[\r\n\x0c]") program
  and blockcomment_depth = ref 0
  and curline = ref 0
  and i = ref 0 in
  (* Small error handler *)
  let lex_error errstr =
    print_string errstr;
    exit 1
  in
  (* Processes each line in input program *)
  let rec do_nextline s =
    (* Matching algorithm for escape sequences in strings. *)
    let match_escape () = () in
    (* Takes the current token, finishes it, and adds it to the queue. Starts a
     * new token of the given type. If the token has fixed (known) length,
     * slurp up the rest of it immediately. *)
    let rec newtok tk =
      (* Check for 1-line comments *)
      if !curlexeme.token = VarSym &&
         is_comment_start (String.sub s
                                      !curlexeme.startpos
                                      (!i - !curlexeme.startpos)) then begin
        (* Symbol token we were going to add actually indicates a start-of-line
         * comment, so just modify the current token (and keep it current). *)
        curlexeme := {!curlexeme with token = Comment};
        (* If this is the end of our string, we then need to pop this comment
         * immediately (means we got called from the top of do_nextline in the
         * process of moving to next line of source) *)
        if !i >= String.length s then
          newtok NullToken
      end
      else begin
        if !curlexeme.token <> NullToken &&
           !curlexeme.token <> Comment &&
           !curlexeme.token <> BlockComment then begin
          curlexeme := {!curlexeme with endpos = !i; endline = !curline};
          Queue.add !curlexeme lexemes
        end;
        curlexeme := { token = tk; startline = !curline; endline = -1;
                       startpos = !i; endpos = -1 };
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
            if s.[!i] <> '\'' then
              lex_error "Character literal too long";
            i := !i + 1;
            newtok NullToken
          end
          | _ -> ();
        ()
      end
    and nextchar_loop () =
      if !i < String.length s then begin
        match !curlexeme.token, s.[!i] with
          (* If we're already inside a 1-line comment, nothing happens *)
          | Comment, _ -> ()
          (* Block comment start, note block comments DO nest *)
          | _, '{' when (!curlexeme.token <> StringLit &&
                         !i + 1 < String.length s && s.[!i+1] = '-') -> begin
            blockcomment_depth := !blockcomment_depth + 1;
            if !blockcomment_depth = 0 then
              newtok BlockComment
          end
          (* Block comment end *)
          | _, '-' when (!curlexeme.token <> StringLit &&
                         !i + 1 < String.length s && s.[!i+1] = '}') -> begin
            blockcomment_depth := !blockcomment_depth - 1;
            if !blockcomment_depth = 0 then
              newtok NullToken
          end
          (* If we're otherwise already inside a block comment, nothing. *)
          | BlockComment, _ -> ()
          (* Process the current character. Note that if the current token
           * continues through this character, we do nothing at all in this
           * matching step. We process fixed-length tokens (char literals and
           * specials) immediately when they start, so curlexeme.token CANNOT
           * be CharLit or Special at this point. *)
          (* Whitespace or special chars ends the current token,
           * except in string literals *)
          | StringLit, c when iswhite(c) || isspecial(c) -> ()
          | _, c when iswhite(c) -> newtok NullToken
          | _, c when isspecial(c) -> newtok Special
          (* Note that digits can appear within varids / conids. Otherwise
           * these arbitrary-length tokens basically end / start whenever the
           * character class changes. *)
          | (NullToken | VarSym | IntLit), ('_' | 'a' .. 'z') ->
              newtok VarId
          | (NullToken | VarSym | IntLit), ('A' .. 'Z') ->
              newtok ConId
          | (NullToken | VarId | ConId | IntLit), c when issymb(c) ->
              newtok VarSym
          | (NullToken | VarSym), ('0' | '1' .. '9') ->
              newtok IntLit
          | (NullToken | VarId | ConId | VarSym | IntLit), '\'' ->
              newtok CharLit
          | (NullToken | VarId | ConId | VarSym | IntLit), '"' ->
              newtok StringLit
          (* String literals: watch out for escaping and end quotes *)
          | StringLit, '"' -> newtok NullToken
          | StringLit, '\\' -> match_escape ()
          (* Otherwise, current token continues, so do nothing *)
          | _ -> ();
        i := !i + 1;
        nextchar_loop ()
      end
    in
    (* End any current token, unless it can span multiple lines. *)
    if !curlexeme.token <> NullToken && !curlexeme.token <> StringLit &&
       !curlexeme.token <> BlockComment then
      newtok NullToken;
    curline := !curline + 1;
    i := 0;
    nextchar_loop ()
  in
  (* Process the whole program, line by line *)
  List.iter do_nextline lines;
  (* Finish the last token (if any), and return *)
  if !curlexeme.token = StringLit then
    lex_error "Unmatched \"";
  if !curlexeme.token = BlockComment then
    lex_error "Unmatched (*";
  curlexeme := {!curlexeme with endpos = !i; endline = !curline};
  Queue.add !curlexeme lexemes;
  lexemes
;;
