open Batteries
;;

let () =
  print_string (dump (List.of_enum (Queue.enum (Lex.prelex "foobar"))))
;;
