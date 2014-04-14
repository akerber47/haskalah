open Batteries
open Types

(** For each (qualified or unqualified) prename, look up the corresponding
 * actual name. Store None as value in case of ambiguity. *)
type environment = (prename, name option) Map.t

(* Convert lexeme to prename. Namespace is context-dependent so can't be
 * determined from lexeme. *)
val lx_to_pn : lexeme -> namespace -> prename

(** Take in an ast and gather all the top-level names. These could be imported
 * or defined at the top-level of this module. *)
val build_globals : ast1 -> environment

(** Using these globals, descend through the AST and convert every identifier
 * to a name of local or global. Assign unique identifiers to local names as we
 * go. *)
val rename : ast1 -> environment -> ast1

(** Using operator fixity declarations, restructure all infixexps and
 * infixpats. This will change each infixexp/pat_op
 * from having one exp/pat10 child and one infixexp/pat child
 * to having two infixexp/pat children. Takes in a renamed ast (ie one with
 * only rleaf leaves) *)
val resolve_fixities : ast1 -> ast1
