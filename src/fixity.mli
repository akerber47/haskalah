open Types

(* Give the fixity system an operator fixity declaration (to use later when
 * resolving fixities). Note that this must be an already-renamed declaration
 * (ie the leaves must be rleaves)
 * ast1 <Ast1_decl_fixity> -> unit *)
val declare : ast1 -> unit

(** Using all operator fixity declarations passed in earlier, restructure all
 * infixexps and infixpats. This will change each infixexp/pat_op from having
 * one exp/pat10 child and one infixexp/pat child to having two infixexp/pat
 * children. Takes in a renamed ast (ie one with only rleaf leaves) *)
val resolve : ast1 -> ast1
