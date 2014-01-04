open Batteries

(* Returns a list of the indices of all elements in the arary which satisfy the
 * predicate, in increasing order. *)
val findi_all : ('a -> bool) -> 'a Array.t -> int list
