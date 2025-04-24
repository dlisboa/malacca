open Malacca

let file_arg = try Some Sys.argv.(1) with _ -> None

let lexbuf =
  match file_arg with
  | Some file -> Lexing.from_channel (open_in file)
  | None -> Lexing.from_channel stdin

let _ =
  while true do
    let prog = Parse.main Lex.next_token lexbuf in
    match prog with
    | Ast.Prog [] -> exit 0
    | _ -> print_endline (Ast.show_program prog)
  done
