open Malacca

let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let prog = Parse.main Lex.next_token lexbuf in
    print_endline (Ast.show_program prog)
  done
