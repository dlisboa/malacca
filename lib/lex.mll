{
  open Parse
}

let whitespace = [' ' '\t']
let digit = ['0'-'9']

rule next_token = parse
  | whitespace+ { next_token lexbuf }
  | digit+ as d { INT (int_of_string d) }
  | eof { EOF }
  | ";" { SEMICOLON }
