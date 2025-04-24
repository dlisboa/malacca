open Malacca

let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let value = Parse.main Lex.next_token lexbuf in
    Printf.printf "%d\n" value;
    print_newline ()
  done
