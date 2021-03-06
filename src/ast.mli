open Types

(* Find the start and end block bounds of a given ast node, given those for all
 * its children. Return the parent ast node with the bounds filled in *)
val ast0_do_bounds : ast0 list -> ast0node -> ast0

(* Helper function to fold over an ast0. The first function is preorder action,
 * second is postorder action. In pseudocode:
 * let ast0_general_foldl pref postf acc ast =
 *   acc1 = pref acc ast <depth>
 *   acc2 = foldl (ast0_general_foldl pref postf) acc1 <children of ast>
 *   postf acc2 ast <depth>
 * ;;
 * *)
val ast0_general_foldl : ('a -> ast0 -> int -> 'a) ->
                         ('a -> ast0 -> int -> 'a) -> 'a -> ast0 -> 'a

(* Similar to above, but only have one of the two actions *)
val ast0_preorder_foldl : ('a -> ast0 -> int -> 'a) -> 'a -> ast0 -> 'a
val ast0_postorder_foldl : ('a -> ast0 -> int -> 'a) -> 'a -> ast0 -> 'a
