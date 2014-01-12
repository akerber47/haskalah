open Types
;;

val computed_do_action : (state * token) -> action
val computed_do_goto : (state * nonterm) -> state
