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
  end
)

open Sheep_parser_gen
;;

let sheep_cfg = {
  goal = SheepNoise;
  productions =
    [| { lhs = SheepNoise;
         rhs = [T Baa; NT SheepNoise];
         semantic_action = (fun [_;i] -> i+1) };
       { lhs = SheepNoise;
         rhs = [T Baa];
         semantic_action = (fun _ -> 1) };
    |];
  terminal_action = (fun _ -> 0);
}

let sheep_parser = generate sheep_cfg
;;
