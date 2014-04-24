open Batteries
open Types

(** For each (qualified or unqualified) prename, look up the corresponding
 * actual name. Store None as value in case of ambiguity. *)
type environment = (prename, name option) Map.t

(** Take in an ast and gather all the top-level names. These could be imported
 * or defined at the top-level of this module. *)
val build_globals : ast1 -> environment

(** Using these globals, descend through the AST and convert every identifier
 * to a name of local or global. Assign unique identifiers to local names as we
 * go. *)
val rename : environment -> ast1 -> ast1

