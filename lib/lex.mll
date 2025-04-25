{
  open Lexing
  open Parse

  exception SyntaxError of string

  let skip_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }

  let keywords = 
    [
        ("int", KEY_INT);
    ]

  let lex_identifier text =
    match List.assoc_opt text keywords with
      | Some tok -> tok
      | None -> IDENTIFIER text
}

let whitespace = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let newline = '\n' | '\r' | "\r\n"

rule next_token = parse
  | whitespace+ { next_token lexbuf }
  | newline { skip_line lexbuf; next_token lexbuf }
  | eof { EOF }

  | alpha alnum* as text { lex_identifier text }

  (* numbers *)
  (* int 1 , float 1., float 1.0, float .01 *)
  | digit+ as d { INT (int_of_string d) }
  | digit+ '.' digit* as text { FLOAT (float_of_string text) }
  | '.' digit+ as text { FLOAT (float_of_string text) }

  (* strings/char *)
  | "'" { lex_char lexbuf }
  | "\"" { lex_string (Buffer.create 64) lexbuf }

  | "int" { KEY_INT }

  (* punctuators *)
  | ";" { SEMICOLON }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQ }
  | _ as c { raise (SyntaxError ("illegal string character: " ^ Char.escaped c)) }

and lex_char = parse
  | _ as c "'" { CHAR c }
  | _ { raise (SyntaxError "illegal char constant")}

and lex_string buf = parse
  | "\"" { STRING (Buffer.contents buf) }
  | '\\' 'n' as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | '\\' 'r' as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | '\\' 't' as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | '\\' '\\' as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | [^ '\\' '"']+ as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | eof { raise (SyntaxError "unterminated string") }
  | _ as c { raise (SyntaxError ("illegal string character: " ^ Char.escaped c)) }
