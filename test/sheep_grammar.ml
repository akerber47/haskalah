open Batteries
;;

(******** Sheep language *********)
(*
 * SheepNoise -> Baa SheepNoise
 *             | Baa
 *)

type sheep_tm =
  | EOF
  | Baa
type sheep_ntm =
  | SheepNoise
  | Goal
type sheep_lx = sheep_tm
type sheep_ast = int

module Sheep_parser_gen = Parser_gen.Make (
  struct
    type tm = sheep_tm
    let tm_compare = compare
    type ntm = sheep_ntm
    let ntm_compare = compare
    type lx = sheep_lx
    let lx_to_tm = identity
    let eof = EOF
    type ast = sheep_ast
    let tm_print =
      (fun o t -> match t with
                  | EOF -> Printf.fprintf o "EOF"
                  | Baa -> Printf.fprintf o "Baa")
    let ntm_print =
      (fun o nt -> match nt with
                   | SheepNoise -> Printf.fprintf o "SheepNoise"
                   | Goal -> Printf.fprintf o "Goal")
    let lx_print = tm_print
    let ast_print = print_guess
  end
)

open Sheep_parser_gen
;;

let sheep_cfg = {
  goal = Goal;
  productions =
    [| { lhs = Goal;
         rhs = [NT SheepNoise];
         semantic_action = List.first; };
       { lhs = SheepNoise;
         rhs = [T Baa; NT SheepNoise];
         semantic_action =
           (fun is ->
             match is with
             | [_;i] -> i+1
             | _ -> assert false) };
       { lhs = SheepNoise;
         rhs = [T Baa];
         semantic_action = (fun _ -> 1) };
    |];
  terminal_action = (fun _ -> 0);
}

let sheep_parser = generate sheep_cfg
;;
