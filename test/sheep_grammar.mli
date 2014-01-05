open Batteries
;;

type sheep_tm =
  | EOF
  | Baa
type sheep_ntm =
  | SheepNoise
type sheep_lx = sheep_tm
type sheep_ast = int

val sheep_parser : sheep_lx Queue.t -> sheep_ast
