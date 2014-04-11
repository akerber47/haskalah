let () =
  ignore(Test_lexer.run_all ());
  ignore(Test_parser.run_all ());
  (*
  ignore(Test_parser_gen.run_all ());
  *)
  ()
;;
