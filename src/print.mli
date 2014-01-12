open Types

(** For debugging and self-modifying code purposes. *)
val token_print : 'a BatIO.output -> token -> unit
val lexeme_print : 'a BatIO.output -> lexeme -> unit
val nonterm_print : 'a BatIO.output -> nonterm -> unit
val ast0_print : 'a BatIO.output -> ast0 -> unit
