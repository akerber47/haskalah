open Batteries
;;

(*
module Foo_pg = Parser_gen.Make (
  struct
    type tm = int
    let tm_compare = compare
    type ntm = int
    let ntm_compare = compare
    type lx = int
    let lx_to_tm = identity
    let eof = 0
    type ast = int
    let tm_print = print_guess
    let ntm_print = print_guess
    let lx_print = print_guess
    let ast_print = print_guess
  end
);;
*)

let () = begin
  (*
  let s = Foo_pg.first_sets
    { Foo_pg.goal = 0;
      Foo_pg.productions = [|
        { Foo_pg.lhs = 0;
          Foo_pg.rhs = [Foo_pg.T 0];
          Foo_pg.semantic_action = (fun _ -> 0); }
      |];
      Foo_pg.terminal_action = (fun _ -> 0); }
  in
  print_string (dump s);
  print_string (dump (Parse.parse (Queue.create ())));
  *)
  print_string (dump (List.of_enum (Queue.enum (Lex.prelex "foobar"))))
end
;;
