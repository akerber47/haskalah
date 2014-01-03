open Batteries
;;

let () = begin
  let s = Parser_gen.ItemCollection.empty in
  print_string (dump s);
  print_string (dump (List.of_enum (Queue.enum (Lex.prelex "foobar"))))
end
;;
