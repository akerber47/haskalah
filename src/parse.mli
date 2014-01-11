open Batteries
open Types

(* Build an abstract syntax tree for the given stream of lexemes. *)
val parse : lexeme Queue.t -> ast0
