{
  open Lexing

  exception SyntaxError of string

  type token =
    | EOF
    | ID of string
    | INT of int
    | FLOAT of float
    | PREPROC of string
    | STRING of string
    | CHAR of char

    | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK | COMMA | SEMI | PLUS | MINUS | HASH
    | STAR | SLASH | BACKSLASH | MOD | DOT | QUESTION | COLON | BANG | TILDE | BIT_AND | BIT_OR | XOR
    | LT | LE | GT | GE | EQ | ARROW | EQ_EQ | NOT_EQ | LOG_AND | LOG_OR | SHIFTL
    | SHIFTR | PLUS_EQ | MINUS_EQ | MULT_EQ | DIV_EQ | MOD_EQ | BIT_AND_EQ | BIT_OR_EQ
    | XOR_EQ | SHIFTL_EQ | SHIFTR_EQ

    | KEY_AUTO | KEY_BREAK | KEY_CASE | KEY_CHAR | KEY_CONST | KEY_CONTINUE | KEY_DEFAULT | KEY_DO
    | KEY_DOUBLE | KEY_ELSE | KEY_ENUM | KEY_EXTERN | KEY_FLOAT | KEY_FOR | KEY_GOTO | KEY_IF
    | KEY_INLINE | KEY_INT | KEY_LONG | KEY_REGISTER | KEY_RESTRICT | KEY_RETURN | KEY_SHORT
    | KEY_SIGNED | KEY_SIZEOF | KEY_STATIC | KEY_STRUCT | KEY_SWITCH | KEY_TYPEDEF | KEY_UNION
    | KEY_UNSIGNED | KEY_VOID | KEY_VOLATILE | KEY_WHILE | KEY__ALIGNAS | KEY__ALIGNOF | KEY__ATOMIC
    | KEY__BOOL | KEY__COMPLEX | KEY__GENERIC | KEY__IMAGINARY | KEY__NORETURN | KEY__STATIC_ASSERT | KEY__THREAD_LOCAL
    [@@deriving show]

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

let lex_identifier lexeme =
  match List.assoc_opt lexeme keywords with
    | Some tok -> tok
    | None -> ID lexeme

let skip_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let digit = ['0'-'9']
let identifier_non_digit = ['a'-'z' 'A'-'Z' '_']
let identifier_digit = ['0'-'9' 'a'-'z' 'A'-'Z' '_']
let identifier = identifier_non_digit identifier_digit*

let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

rule next_token = parse
  | eof { EOF }
  | whitespace { next_token lexbuf }
  | newline { skip_line lexbuf; next_token lexbuf }

  | "//" { lex_line_comment lexbuf }
  | "/*" { lex_block_comment lexbuf }

  (* identifiers *)
  | identifier as lexeme { lex_identifier lexeme }

  (* numbers *)
  | digit+ as lexeme { INT (int_of_string lexeme) }
  | digit+? '.' digit* as lexeme { FLOAT (float_of_string lexeme) }

  (* string/char *)
  | '\'' { lex_char lexbuf }
  | '"' { lex_string (Buffer.create 64) lexbuf }

  (* preprocessor *)
  | '#' identifier [^ '\n']+ as lexeme { PREPROC lexeme }

  (* punctuators *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | ';' { SEMI }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '\\' { BACKSLASH }
  | '%' { MOD }
  | '.' { DOT }
  | '?' { QUESTION }
  | ':' { COLON }
  | '!' { BANG }
  | '~' { TILDE }
  | '&' { BIT_AND }
  | '|' { BIT_OR }
  | '^' { XOR }
  | '<' { LT }
  | "<=" { LE }
  | '>' { GT }
  | ">=" { GE }
  | '=' { EQ }
  | "->" { ARROW }
  | "==" { EQ_EQ }
  | "!=" { NOT_EQ }
  | "&&" { LOG_AND }
  | "||" { LOG_OR }
  | "<<" { SHIFTL }
  | ">>" { SHIFTR }
  | "+=" { PLUS_EQ }
  | "-=" { MINUS_EQ }
  | "*=" { MULT_EQ }
  | "/=" { DIV_EQ }
  | "%=" { MOD_EQ }
  | "&=" { BIT_AND_EQ }
  | "|=" { BIT_OR_EQ }
  | "^=" { XOR_EQ }
  | "<<=" { SHIFTL_EQ }
  | ">>=" { SHIFTR_EQ }

  (* rest *)
  | _ as char { raise (SyntaxError ("illegal character: '" ^ (Char.escaped char) ^ "'")) }

and lex_char = parse
  | _ as c '\'' { CHAR c }
  | _ { raise (SyntaxError "illegal char constant")}

and lex_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
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

