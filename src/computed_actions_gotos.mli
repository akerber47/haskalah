open Batteries
;;
open Types
;;

val computed_do_action : (state * token) -> action
val computed_goto_action : (state * nonterm) -> state
