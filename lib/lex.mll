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
      ("auto", KEY_AUTO);
      ("break", KEY_BREAK);
      ("case", KEY_CASE);
      ("char", KEY_CHAR);
      ("const", KEY_CONST);
      ("continue", KEY_CONTINUE);
      ("default", KEY_DEFAULT);
      ("do", KEY_DO);
      ("double", KEY_DOUBLE);
      ("else", KEY_ELSE);
      ("enum", KEY_ENUM);
      ("extern", KEY_EXTERN);
      ("float", KEY_FLOAT);
      ("for", KEY_FOR);
      ("goto", KEY_GOTO);
      ("if", KEY_IF);
      ("inline", KEY_INLINE);
      ("int", KEY_INT);
      ("long", KEY_LONG);
      ("register", KEY_REGISTER);
      ("restrict", KEY_RESTRICT);
      ("return", KEY_RETURN);
      ("short", KEY_SHORT);
      ("signed", KEY_SIGNED);
      ("sizeof", KEY_SIZEOF);
      ("static", KEY_STATIC);
      ("struct", KEY_STRUCT);
      ("switch", KEY_SWITCH);
      ("typedef", KEY_TYPEDEF);
      ("union", KEY_UNION);
      ("unsigned", KEY_UNSIGNED);
      ("void", KEY_VOID);
      ("volatile", KEY_VOLATILE);
      ("while", KEY_WHILE);
      ("_Alignas", KEY__ALIGNAS);
      ("_Alignof", KEY__ALIGNOF);
      ("_Atomic", KEY__ATOMIC);
      ("_Bool", KEY__BOOL);
      ("_Complex", KEY__COMPLEX);
      ("_Generic", KEY__GENERIC);
      ("_Imaginary", KEY__IMAGINARY);
      ("_Noreturn", KEY__NORETURN);
      ("_Static_assert", KEY__STATIC_ASSERT);
      ("_Thread_local", KEY__THREAD_LOCAL);
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

  (* comments *)
  | "//" { lex_line_comment lexbuf }
  | "/*" { lex_block_comment lexbuf }

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

and lex_line_comment = parse
  | eof { EOF }
  | newline { skip_line lexbuf; next_token lexbuf }
  | _ { lex_line_comment lexbuf }

and lex_block_comment = parse
  | "*/" { next_token lexbuf }
  | eof { raise (SyntaxError "unterminated block comment") }
  | newline { skip_line lexbuf; lex_block_comment lexbuf }
  | _ { lex_block_comment lexbuf }

