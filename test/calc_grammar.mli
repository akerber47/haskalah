open Batteries
;;

type calc_tm =
  | EOF
  | Plus
  | Times
  | Num
  | LParen
  | RParen
type calc_ntm =
  | Goal
  | Expr
  | Term
  | Factor

type calc_lx = calc_tm * int

type calc_ast = int

val calc_parser : calc_lx Queue.t -> calc_ast
