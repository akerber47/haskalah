open Batteries
;;

(******** Calc language *********)
(* Arithmetic expressions with + and * operator precedence and parens. *)

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

module Calc_parser_gen = Parser_gen.Make (
  struct
    type tm = calc_tm
    let tm_compare = compare
    type ntm = calc_ntm
    let ntm_compare = compare
    type lx = calc_lx
    let lx_to_tm (t,_) = t
    let eof = EOF
    type ast = calc_ast
    let tm_print o t =
      match t with
      | EOF -> Printf.fprintf o "EOF"
      | Plus -> Printf.fprintf o "Plus"
      | Times -> Printf.fprintf o "Times"
      | Num -> Printf.fprintf o "Num"
      | LParen -> Printf.fprintf o "LParen"
      | RParen -> Printf.fprintf o "RParen"
    let ntm_print o nt =
      match nt with
      | Goal -> Printf.fprintf o "Goal"
      | Expr -> Printf.fprintf o "Expr"
      | Term -> Printf.fprintf o "Term"
      | Factor -> Printf.fprintf o "Factor"
    let lx_print o (t,n) = Printf.fprintf o "(%a,%d)" tm_print t n
    let ast_print = print_guess
  end
)

open Calc_parser_gen
;;

let calc_cfg = {
  goal = Goal;
  productions =
    [|  { lhs = Goal;
          rhs = [NT Expr];
          semantic_action = List.first; };
        { lhs = Expr;
          rhs = [NT Expr; T Plus; NT Term];
          semantic_action =
            (fun ns ->
              match ns with
              | [n1;_;n2] -> n1+n2
              | _ -> assert false) };
        { lhs = Expr;
          rhs = [NT Term];
          semantic_action = List.first; };
        { lhs = Term;
          rhs = [NT Term; T Times; NT Factor];
          semantic_action =
            (fun ns ->
              match ns with
              | [n1;_;n2] -> n1*n2
              | _ -> assert false) };
        { lhs = Term;
          rhs = [NT Factor];
          semantic_action = List.first; };
        { lhs = Factor;
          rhs = [T LParen; NT Expr; T RParen];
          semantic_action =
            (fun ns ->
              match ns with
              | [_;n;_] -> n
              | _ -> assert false) };
        { lhs = Factor;
          rhs = [T Num];
          semantic_action = List.first; };
    |];
  terminal_action = (fun (_,n) -> n);
}

let calc_parser = generate calc_cfg
;;
