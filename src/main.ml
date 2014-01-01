open Batteries
;;

let () =
  dump (List.of_enum (Queue.enum (Lex.prelex "foobar")))
;;
