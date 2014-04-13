open Batteries
open Types

(** For each (qualified or unqualified) identifier, look up the module in which
 * it was *really* defined - not necessarily the one that most recently
 * exported it. *)
type globals = {
  unqualified : (string, string) Map.t;
  qualified : (string * string, string) Map.t
}

(** Take in an ast and gather all the top-level names. These could be imported
 * or defined at the top-level of this module *)
val build_globals : ast1 -> globals

(** Using these globals, descend through the AST and convert every identifier
 * to a name of local or global. Assign unique identifiers to local names as we
 * go. *)
val rename : ast1 -> ast1

(** Using operator fixity declarations, restructure all infixexps and
 * infixpats. This will change each infixexp/pat_op
 * from having one exp/pat10 child and one infixexp/pat child
 * to having two infixexp/pat children. *)
val resolve_fixities : ast1 -> ast1
