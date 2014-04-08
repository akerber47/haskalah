open Types

(** Check 2 things that should have checked in the parser, except that
 * they'd make our grammar hideous, ambiguous, undecidable, or all of the
 * above. We verify that
 * * type contexts appear only where allowed, and are grammatically correct
 *   (simultaneously, we convert type -> [context] type where appropriate)
 * * patterns appear where required, and are grammatically correct
 *   (simultaneously, we convert exp -> pat where appropriate)
 * We store these changes in a new AST format. *)
val postparse_check : ast0 -> ast0
