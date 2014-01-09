open Batteries
;;

(* Given filename, reads in the entire file as a string *)
val file_to_string : string -> string

(* Prints a debug statement to stderr, if debug is on *)
val dbg : ('a, 'b BatInnerIO.output, unit, unit) format4 -> 'a

(* Returns a list of the indices of all elements in the arary which satisfy the
 * predicate, in increasing order. *)
val findi_all : ('a -> bool) -> 'a Array.t -> int list
