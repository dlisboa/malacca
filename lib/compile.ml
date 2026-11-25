(* The compiler pipeline, minus anything that touches the outside world
   (running the assembler, writing files...). That part lives in bin/main.ml.

     source text --preprocess--> text --parse--> Ast.program --Gen.program--> assembly

   Every user-facing failure is reported by raising [Error] with a message that
   is ready to print. *)

exception Error of string

(* "file.c:3:14" for the token the lexer most recently started reading. *)
let position lexbuf =
  let pos = lexbuf.Lexing.lex_start_p in
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(* Run the C preprocessor over [source] (the text of [filename]). *)
let preprocess ~include_dirs ~filename source =
  try Preproc.run ~include_dirs ~filename source
  with Preproc.Error msg -> raise (Error msg)

let parse ~filename source =
  let lexbuf = Lexing.from_string source in
  (* Lexing.from_string doesn't know the file name; record it for messages. *)
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try Parse.main Lex.next_token lexbuf with
  | Lex.SyntaxError msg ->
      raise (Error (Printf.sprintf "%s: error: %s" (position lexbuf) msg))
  | Parsing.Parse_error ->
      raise (Error (Printf.sprintf "%s: error: syntax error" (position lexbuf)))

let to_assembly program =
  try Gen.program program with Gen.Error msg -> raise (Error msg)
