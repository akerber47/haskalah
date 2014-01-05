let () =
  ignore(Test_lex.run_all ());
  ignore(Test_parser_gen.run_all ());
  ()
;;
