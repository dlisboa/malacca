let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let tok = Lex.next_token lexbuf in
      (*print_endline (Lex.show_token tok)*)
      print_endline (Lex.show_token tok)
    done
  with Lex.Eof -> exit 0
