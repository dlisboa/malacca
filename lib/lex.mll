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

  (* An integer constant that doesn't fit in an OCaml int is a lexing error
     rather than a crash. *)
  let int_constant text =
    match int_of_string_opt text with
    | Some n -> INT n
    | None -> raise (SyntaxError ("integer constant is too large: " ^ text))

  let lex_identifier text =
    match List.assoc_opt text keywords with
      | Some tok -> tok
      | None -> IDENTIFIER text
}

let whitespace = [' ' '\t']
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let oct_digit = ['0'-'7']
(* Integer suffixes (1L, 1u, 1UL...) are accepted and ignored. *)
let int_suffix = ['u' 'U' 'l' 'L']*
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let newline = '\n' | '\r' | "\r\n"

rule next_token = parse
  | whitespace+ { next_token lexbuf }

  (* The preprocessor leaves markers such as  # 12 "stdio.h"  meaning "the next
     line is line 12 of stdio.h". They are the only '#' left in the text. *)
  | "# " (digit+ as line) " \"" ([^ '"' '\n']* as file) "\"" [^ '\n']* newline {
      let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_fname = file; pos_lnum = int_of_string line;
                   pos_bol = lexbuf.lex_curr_pos };
      next_token lexbuf
    }
  | newline { skip_line lexbuf; next_token lexbuf }
  | eof { EOF }

  | alpha alnum* as text { lex_identifier text }

  (* numbers *)
  (* int 1 , float 1., float 1.0, float .01 *)
  (* "0x" is OCaml's own hex prefix, so int_of_string can read it directly. *)
  | ("0" ['x' 'X'] hex_digit+) as h int_suffix { int_constant h }
  (* C octal literals start with 0 (017 = 15); OCaml spells that "0o17". *)
  | "0" (oct_digit+ as o) int_suffix { int_constant ("0o" ^ o) }
  | (digit+ as d) int_suffix { int_constant d }
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
  | "." { DOT }
  | "," { COMMA }
  | "?" { QUESTION }
  | ":" { COLON }
  | "!" { BANG }
  | "~" { TILDE }
  | "%" { MOD }
  | "&" { BIT_AND }
  | "|" { BIT_OR }
  | "^" { XOR }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
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
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "++" { INC }
  | "--" { DEC }
  | "..." { ELLIPSIS }

  | _ as c { raise (SyntaxError ("illegal character: " ^ Char.escaped c)) }

(* A character constant; the opening quote has been consumed. The value is the
   byte itself (so '\n' is the char with code 10). *)
and lex_char = parse
  | '\\' { let c = lex_escape lexbuf in close_char lexbuf; CHAR c }
  | [^ '\\' '\'' '\n'] as c { close_char lexbuf; CHAR c }
  | _ { raise (SyntaxError "invalid character constant") }

and close_char = parse
  | "'" { () }
  | _ { raise (SyntaxError "character constant is too long or unterminated") }

(* The text after a backslash, inside a char or string literal. Returns the
   byte it stands for: newline, tab, backslash, a hex escape such as x41, an
   octal escape such as 101, and so on. *)
and lex_escape = parse
  | 'n' { '\n' }
  | 't' { '\t' }
  | 'r' { '\r' }
  | 'a' { '\007' }
  | 'b' { '\b' }
  | 'f' { '\012' }
  | 'v' { '\011' }
  | '\\' { '\\' }
  | '\'' { '\'' }
  | '"' { '"' }
  | '?' { '?' }
  | (oct_digit oct_digit? oct_digit?) as o { Char.chr (int_of_string ("0o" ^ o) land 255) }
  | 'x' (hex_digit+ as h) { Char.chr (int_of_string ("0x" ^ h) land 255) }
  | _ as c { raise (SyntaxError (Printf.sprintf "unknown escape sequence '\\%c'" c)) }

(* A string literal; the opening quote has been consumed. Escapes are decoded as
   we go, so the token carries the real bytes of the string. *)
and lex_string buf = parse
  | "\"" { STRING (Buffer.contents buf) }
  | '\\' { Buffer.add_char buf (lex_escape lexbuf); lex_string buf lexbuf }
  | [^ '\\' '"' '\n']+ as text { Buffer.add_string buf text; lex_string buf lexbuf }
  | newline | eof { raise (SyntaxError "unterminated string literal") }

and lex_line_comment = parse
  | eof { EOF }
  | newline { skip_line lexbuf; next_token lexbuf }
  | _ { lex_line_comment lexbuf }

and lex_block_comment = parse
  | "*/" { next_token lexbuf }
  | eof { raise (SyntaxError "unterminated block comment") }
  | newline { skip_line lexbuf; lex_block_comment lexbuf }
  | _ { lex_block_comment lexbuf }

