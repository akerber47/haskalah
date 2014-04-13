(* A simple unique id generator. Uses shared state and mutation, careful! Takes
 * in a strings and generates a new unique id (for that string) each time. *)

val get : string -> int

val reset : unit -> unit

