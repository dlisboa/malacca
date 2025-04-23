open Malacca

let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    match Lex.next_token lexbuf with
    | Lex.EOF -> exit 0
    | tok -> print_endline (Lex.show_token tok)
  done
