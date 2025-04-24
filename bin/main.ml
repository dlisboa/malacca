open Malacca

let file_arg = try Some Sys.argv.(1) with _ -> None

let lexbuf =
  match file_arg with
  | Some file -> Lexing.from_channel (open_in file)
  | None -> Lexing.from_channel stdin

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let _ =
  try
    let prog = Parse.main Lex.next_token lexbuf in
    print_endline (Ast.show_program prog)
  with Parsing.Parse_error ->
    let pos = print_position lexbuf in
    Printf.eprintf "Parse error at %s\n" pos;
    exit 1
