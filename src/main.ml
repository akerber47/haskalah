open Batteries
;;

module FooParserGenerator = Parser_gen.Make (
  struct
    type tm = int
    let tm_compare = compare
    type ntm = int
    let ntm_compare = compare
    type lx = int
    let lx_to_tm = identity
    type ast = int
    let eof = 0
  end
);;

let () = begin
  let s = FooParserGenerator.Item_set.empty in
  print_string (dump s);
  print_string (dump (List.of_enum (Queue.enum (Lex.prelex "foobar"))))
end
;;
