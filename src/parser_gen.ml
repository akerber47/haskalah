(* Unfortunately this Haskell grammar fails very, very badly at being LL(1)
 * (it has lots and lots of infix operators, can't tell tuples from parentheses
 * or lists from list comprehensions, etc.). It might be convertible to LL(1)
 * but that would take tons of factoring. So we'll implement bottom-up parsing.
 * LR(1). *)

(* Items in the LR(1) construction *)
type item = {
  prod : int;
  dot : int;
  lookahead : Parse.term;
};;

module ItemSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = item
  end
);;

module ItemCollection = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = ItemSet.t * int
  end
);;
