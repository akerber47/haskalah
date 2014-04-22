open Types

(** Check 5 things that should have checked in the parser, except that
 * they'd make our grammar hideous, ambiguous, undecidable, or all of the
 * above. We verify that
 * * type contexts appear only where allowed, and are grammatically correct
 *   (simultaneously, we convert type -> [context] type where appropriate)
 * * patterns appear where required, and are grammatically correct
 *   (simultaneously, we convert exp -> pat where appropriate)
 * * import declarations actually use the correct words 'qualified', 'as',
 *   and/or 'hiding'.
 * * data constructors actually consist of a valid constructor followed by a
 *   bunch of atypes. eg they can't start with a type variable
 *   (convert btype -> con atype* where appropriate)
 * * Only valid cdecls / idecls are used in class / instance bodies.
 * We store these changes in a new AST format. *)
val postparse_check : ast0 -> ast1
