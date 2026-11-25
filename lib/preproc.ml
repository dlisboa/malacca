(* The C preprocessor: the text-to-text pass that runs before the lexer.

   It handles
     #include "file" / <file>        paste another file in
     #define NAME body               object-like macros
     #define NAME(a, b) body         function-like macros
     #undef NAME
     #if / #ifdef / #ifndef / #elif / #else / #endif
     #error                          (#pragma and #line are ignored)

   It does NOT support: the # and ## operators, variadic macros, macros whose
   arguments span several lines, #include with a macro operand, #pragma once.
   See docs/preprocessor.md.

   The output keeps line numbers meaningful: every input line produces exactly
   one output line (blank for directives and skipped code), and each #include is
   wrapped in "# <line> "<file>"" markers that the lexer understands, so error
   messages point at the right file and line. *)

exception Error of string

type macro =
  | Object of string  (* #define N 10 *)
  | Function of string list * string  (* #define SQ(x) ((x)*(x)): params, body *)

(* One level of #if nesting. *)
type cond = {
  parent_active : bool;  (* was the enclosing region being compiled? *)
  mutable taken : bool;  (* has some branch of this #if already been used? *)
  mutable active : bool;  (* is the current branch being compiled? *)
}

type state = {
  macros : (string, macro) Hashtbl.t;
  include_dirs : string list;
  out : Buffer.t;
}

(* Where we are in the input: used for __LINE__ and error messages. *)
type position = { file : string; mutable line : int }

let fail pos fmt =
  Printf.ksprintf
    (fun msg -> raise (Error (Printf.sprintf "%s:%d: error: %s" pos.file pos.line msg)))
    fmt

(* ------------------------------------------------------------------------ *)
(* Small text helpers                                                       *)
(* ------------------------------------------------------------------------ *)

let is_ident_start c = c = '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_ident_char c = is_ident_start c || is_digit c
let is_space c = c = ' ' || c = '\t' || c = '\r'

(* Index of the first character at or after [i] that does not satisfy [pred]. *)
let skip_while pred s i =
  let n = String.length s in
  let rec go i = if i < n && pred s.[i] then go (i + 1) else i in
  go i

(* Copy the string or character literal that starts at [s.[i]] into [b] and
   return the index after it. Macros must not be expanded inside literals. *)
let copy_literal b s i =
  let n = String.length s in
  let quote = s.[i] in
  Buffer.add_char b quote;
  let rec go i =
    if i >= n then i
    else
      let c = s.[i] in
      Buffer.add_char b c;
      if c = '\\' && i + 1 < n then (
        Buffer.add_char b s.[i + 1];
        go (i + 2))
      else if c = quote then i + 1
      else go (i + 1)
  in
  go (i + 1)

(* ------------------------------------------------------------------------ *)
(* Comments and line continuations                                          *)
(* ------------------------------------------------------------------------ *)

(* Replace every comment by a single space (or, for a block comment, by as many
   newlines as it contained, so line numbers don't shift). String and character
   literals are skipped over so that "http://x" is left alone. *)
let strip_comments text =
  let n = String.length text in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = text.[!i] in
    if c = '"' || c = '\'' then begin
      (* An unterminated literal ends at the end of the line. *)
      let line_end = try String.index_from text !i '\n' with Not_found -> n in
      let literal = Buffer.create 16 in
      let stop = copy_literal literal text !i in
      if stop > line_end then (
        Buffer.add_string b (String.sub text !i (line_end - !i));
        i := line_end)
      else (
        Buffer.add_buffer b literal;
        i := stop)
    end
    else if c = '/' && !i + 1 < n && text.[!i + 1] = '/' then
      (* line comment: drop up to, but not including, the newline *)
      i := skip_while (fun c -> c <> '\n') text !i
    else if c = '/' && !i + 1 < n && text.[!i + 1] = '*' then begin
      Buffer.add_char b ' ';
      i := !i + 2;
      while !i < n && not (text.[!i] = '*' && !i + 1 < n && text.[!i + 1] = '/') do
        if text.[!i] = '\n' then Buffer.add_char b '\n';
        incr i
      done;
      if !i >= n then raise (Error "unterminated /* comment");
      i := !i + 2
    end
    else begin
      Buffer.add_char b c;
      incr i
    end
  done;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)
(* Macro expansion                                                          *)
(* ------------------------------------------------------------------------ *)

(* Parse the arguments of a function-like macro call. [i] is the index just
   after the opening parenthesis. Returns the arguments (split at top-level
   commas, so f(a, g(b, c)) has two) and the index after the closing one. *)
let parse_args pos s i =
  let n = String.length s in
  let args = ref [] and current = Buffer.create 16 in
  let rec go i depth =
    if i >= n then fail pos "unterminated macro argument list"
    else
      match s.[i] with
      | '"' | '\'' -> go (copy_literal current s i) depth
      | '(' ->
          Buffer.add_char current '(';
          go (i + 1) (depth + 1)
      | ')' when depth = 0 -> i + 1
      | ')' ->
          Buffer.add_char current ')';
          go (i + 1) (depth - 1)
      | ',' when depth = 0 ->
          args := Buffer.contents current :: !args;
          Buffer.clear current;
          go (i + 1) depth
      | c ->
          Buffer.add_char current c;
          go (i + 1) depth
  in
  let after = go i 0 in
  args := Buffer.contents current :: !args;
  (List.rev_map String.trim !args, after)

(* Replace each use of a parameter name in a macro body with its argument. *)
let substitute body params args =
  let n = String.length body in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = body.[!i] in
    if is_ident_start c then begin
      let j = skip_while is_ident_char body !i in
      let word = String.sub body !i (j - !i) in
      (match List.find_opt (fun (p, _) -> p = word) (List.combine params args) with
      | Some (_, arg) -> Buffer.add_string b arg
      | None -> Buffer.add_string b word);
      i := j
    end
    else if c = '"' || c = '\'' then i := copy_literal b body !i
    else begin
      Buffer.add_char b c;
      incr i
    end
  done;
  Buffer.contents b

(* Expand all macros in [text]. [hidden] lists the macros currently being
   expanded: a macro's own name inside its expansion is left alone, which is
   what stops "#define X X + 1" from looping forever. *)
let rec expand st pos ~hidden text =
  let n = String.length text in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = text.[!i] in
    if is_ident_start c then begin
      let j = skip_while is_ident_char text !i in
      let name = String.sub text !i (j - !i) in
      i := j;
      match name with
      | "__LINE__" -> Buffer.add_string b (string_of_int pos.line)
      | "__FILE__" -> Buffer.add_string b (Printf.sprintf "%S" pos.file)
      | _ -> (
          match Hashtbl.find_opt st.macros name with
          | Some (Object body) when not (List.mem name hidden) ->
              Buffer.add_string b (expand st pos ~hidden:(name :: hidden) body)
          | Some (Function (params, body)) when not (List.mem name hidden) ->
              (* A function-like macro is only a call if "(" follows. *)
              let k = skip_while is_space text !i in
              if k < n && text.[k] = '(' then begin
                let args, after = parse_args pos text (k + 1) in
                (* "f()" has one empty argument, which is fine for f(void-ish) *)
                let args = if params = [] && args = [ "" ] then [] else args in
                if List.length args <> List.length params then
                  fail pos "macro '%s' takes %d argument(s) but %d given" name
                    (List.length params) (List.length args);
                i := after;
                let args = List.map (expand st pos ~hidden) args in
                Buffer.add_string b
                  (expand st pos ~hidden:(name :: hidden) (substitute body params args))
              end
              else Buffer.add_string b name
          | _ -> Buffer.add_string b name)
    end
    else if is_digit c then begin
      (* A number like 0x1F or 1e5 is one unit: don't look for macros in it. *)
      let j = skip_while (fun c -> is_ident_char c || c = '.') text !i in
      Buffer.add_string b (String.sub text !i (j - !i));
      i := j
    end
    else if c = '"' || c = '\'' then i := copy_literal b text !i
    else begin
      Buffer.add_char b c;
      incr i
    end
  done;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)
(* #if expressions                                                          *)
(* ------------------------------------------------------------------------ *)

(* Replace "defined X" and "defined(X)" by 1 or 0. This must happen before
   macro expansion, or X would be expanded away first. *)
let replace_defined st text =
  let n = String.length text in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if is_ident_start text.[!i] then begin
      let j = skip_while is_ident_char text !i in
      let word = String.sub text !i (j - !i) in
      if word = "defined" then begin
        let k = skip_while is_space text j in
        let paren = k < n && text.[k] = '(' in
        let k = skip_while is_space text (if paren then k + 1 else k) in
        let e = skip_while is_ident_char text k in
        let name = String.sub text k (e - k) in
        let e = skip_while is_space text e in
        let e = if paren && e < n && text.[e] = ')' then e + 1 else e in
        Buffer.add_string b (if Hashtbl.mem st.macros name then "1" else "0");
        i := e
      end
      else (
        Buffer.add_string b word;
        i := j)
    end
    else (
      Buffer.add_char b text.[!i];
      incr i)
  done;
  Buffer.contents b

type token = Num of int | Op of string

let tokenize pos text =
  let n = String.length text in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = text.[i] in
      if is_space c then go (i + 1) acc
      else if is_digit c then begin
        let j = skip_while is_ident_char text i in
        (* Drop integer suffixes such as 10UL before converting. *)
        let digits = String.sub text i (j - i) in
        let k = ref (String.length digits) in
        while !k > 1 && (match digits.[!k - 1] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false) do
          decr k
        done;
        match int_of_string_opt (String.sub digits 0 !k) with
        | Some v -> go j (Num v :: acc)
        | None -> fail pos "invalid number '%s' in #if" digits
      end
      else if is_ident_start c then
        (* An identifier that survived macro expansion is 0, as in C. *)
        go (skip_while is_ident_char text i) (Num 0 :: acc)
      else
        let two = if i + 1 < n then String.sub text i 2 else "" in
        if List.mem two [ "||"; "&&"; "=="; "!="; "<="; ">="; "<<"; ">>" ] then
          go (i + 2) (Op two :: acc)
        else go (i + 1) (Op (String.make 1 c) :: acc)
  in
  go 0 []

(* Binary operator precedence for #if; 0 means "not a binary operator". *)
let precedence = function
  | "||" -> 1 | "&&" -> 2 | "|" -> 3 | "^" -> 4 | "&" -> 5
  | "==" | "!=" -> 6
  | "<" | ">" | "<=" | ">=" -> 7
  | "<<" | ">>" -> 8
  | "+" | "-" -> 9
  | "*" | "/" | "%" -> 10
  | _ -> 0

let eval_condition st pos text =
  let toks = ref (tokenize pos (expand st pos ~hidden:[] (replace_defined st text))) in
  let peek () = match !toks with t :: _ -> Some t | [] -> None in
  let advance () = toks := List.tl !toks in
  let bool b = if b then 1 else 0 in
  let apply op x y =
    match op with
    | "||" -> bool (x <> 0 || y <> 0) | "&&" -> bool (x <> 0 && y <> 0)
    | "|" -> x lor y | "^" -> x lxor y | "&" -> x land y
    | "==" -> bool (x = y) | "!=" -> bool (x <> y)
    | "<" -> bool (x < y) | ">" -> bool (x > y)
    | "<=" -> bool (x <= y) | ">=" -> bool (x >= y)
    | "<<" -> x lsl y | ">>" -> x asr y
    | "+" -> x + y | "-" -> x - y | "*" -> x * y
    | "/" | "%" -> if y = 0 then fail pos "division by zero in #if" else if op = "/" then x / y else x mod y
    | _ -> fail pos "bad operator '%s' in #if" op
  in
  let rec unary () =
    match peek () with
    | Some (Num v) -> advance (); v
    | Some (Op "(") ->
        advance ();
        let v = ternary () in
        (match peek () with
         | Some (Op ")") -> advance ()
         | _ -> fail pos "missing ')' in #if");
        v
    | Some (Op "!") -> advance (); bool (unary () = 0)
    | Some (Op "-") -> advance (); - (unary ())
    | Some (Op "+") -> advance (); unary ()
    | Some (Op "~") -> advance (); lnot (unary ())
    | _ -> fail pos "bad expression in #if"
  (* Precedence climbing: parse operators binding at least as tightly as [min]. *)
  and binary min =
    let lhs = ref (unary ()) in
    let go = ref true in
    while !go do
      match peek () with
      | Some (Op op) when precedence op >= min && precedence op > 0 ->
          advance ();
          let rhs = binary (precedence op + 1) in
          lhs := apply op !lhs rhs
      | _ -> go := false
    done;
    !lhs
  and ternary () =
    let c = binary 1 in
    match peek () with
    | Some (Op "?") ->
        advance ();
        let a = ternary () in
        (match peek () with
         | Some (Op ":") -> advance ()
         | _ -> fail pos "missing ':' in #if");
        let b = ternary () in
        if c <> 0 then a else b
    | _ -> c
  in
  let v = ternary () in
  if !toks <> [] then fail pos "unexpected tokens at the end of #if";
  v <> 0

(* ------------------------------------------------------------------------ *)
(* Directives and the main loop                                             *)
(* ------------------------------------------------------------------------ *)

let read_file path =
  try Some (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> None

(* Find the file named by an #include. Order: the including file's directory
   (quoted form only), then -I directories, then the headers built into the
   compiler. Returns the name to show in messages, and the contents. *)
let find_include st pos ~quoted ~from_dir name =
  let dirs = (if quoted then [ from_dir ] else []) @ st.include_dirs in
  let rec try_dirs = function
    | [] -> None
    | dir :: rest -> (
        let path = Filename.concat dir name in
        match read_file path with
        | Some text -> Some (path, text)
        | None -> try_dirs rest)
  in
  match try_dirs dirs with
  | Some found -> found
  | None -> (
      match Builtin_headers.find name with
      | Some text -> ("<" ^ name ^ ">", text)
      | None -> fail pos "'%s' file not found" name)

(* Parse "NAME body" or "NAME(a, b) body" after #define. *)
let define st pos arg =
  let j = skip_while is_ident_char arg 0 in
  if j = 0 || not (is_ident_start arg.[0]) then fail pos "macro name missing";
  let name = String.sub arg 0 j in
  if j < String.length arg && arg.[j] = '(' then begin
    (* function-like only when "(" touches the name: "#define F (x)" is
       an object-like macro whose body is "(x)". *)
    let close =
      try String.index_from arg j ')' with Not_found -> fail pos "missing ')' in macro parameters"
    in
    let params =
      String.sub arg (j + 1) (close - j - 1)
      |> String.split_on_char ',' |> List.map String.trim
      |> List.filter (fun p -> p <> "")
    in
    if List.mem "..." params then fail pos "variadic macros are not supported";
    let body = String.trim (String.sub arg (close + 1) (String.length arg - close - 1)) in
    Hashtbl.replace st.macros name (Function (params, body))
  end
  else
    Hashtbl.replace st.macros name
      (Object (String.trim (String.sub arg j (String.length arg - j))))

let max_include_depth = 64

(* Process the text of one file, appending the result to [st.out]. *)
let rec process_file st ~depth ~file text =
  if depth > max_include_depth then raise (Error (file ^ ": #include nested too deeply"));
  let pos = { file; line = 0 } in
  let conds : cond list ref = ref [] in
  let active () = match !conds with [] -> true | c :: _ -> c.active in
  let from_dir = Filename.dirname file in
  Buffer.add_string st.out (Printf.sprintf "# 1 \"%s\"\n" file);
  let lines = Array.of_list (String.split_on_char '\n' (strip_comments text)) in
  let total = Array.length lines in
  let i = ref 0 in
  while !i < total do
    (* Join lines ending in a backslash into one logical line. *)
    let first = !i + 1 in
    let logical = Buffer.create 80 in
    let continues l = String.length l > 0 && l.[String.length l - 1] = '\\' in
    while continues lines.(!i) && !i + 1 < total do
      let l = lines.(!i) in
      Buffer.add_string logical (String.sub l 0 (String.length l - 1));
      incr i
    done;
    Buffer.add_string logical lines.(!i);
    pos.line <- first;
    let line = Buffer.contents logical in
    let trimmed = String.trim line in
    if trimmed <> "" && trimmed.[0] = '#' then begin
      let directive = String.trim (String.sub trimmed 1 (String.length trimmed - 1)) in
      let k = skip_while is_ident_char directive 0 in
      let name = String.sub directive 0 k in
      let arg = String.trim (String.sub directive k (String.length directive - k)) in
      directive_line st pos ~depth ~from_dir ~conds ~active name arg;
      (* An #include writes its own lines; any other directive becomes blank. *)
      if name <> "include" || not (active ()) then Buffer.add_char st.out '\n'
    end
    else begin
      if active () then Buffer.add_string st.out (expand st pos ~hidden:[] line);
      Buffer.add_char st.out '\n'
    end;
    (* Keep the line count: a spliced logical line still spans several lines. *)
    for _ = first to !i do
      Buffer.add_char st.out '\n'
    done;
    incr i
  done;
  if !conds <> [] then fail { pos with line = total } "unterminated #if"

and directive_line st pos ~depth ~from_dir ~conds ~active name arg =
  let cond_stack_top what =
    match !conds with c :: _ -> c | [] -> fail pos "#%s without #if" what
  in
  match name with
  | "ifdef" | "ifndef" ->
      let defined = Hashtbl.mem st.macros arg in
      let parent_active = active () in
      let on = parent_active && if name = "ifdef" then defined else not defined in
      conds := { parent_active; taken = on; active = on } :: !conds
  | "if" ->
      let parent_active = active () in
      let on = parent_active && eval_condition st pos arg in
      conds := { parent_active; taken = on; active = on } :: !conds
  | "elif" ->
      let c = cond_stack_top "elif" in
      if c.parent_active && not c.taken && eval_condition st pos arg then (
        c.active <- true;
        c.taken <- true)
      else c.active <- false
  | "else" ->
      let c = cond_stack_top "else" in
      c.active <- c.parent_active && not c.taken;
      c.taken <- true
  | "endif" ->
      ignore (cond_stack_top "endif");
      conds := List.tl !conds
  | _ when not (active ()) -> ()  (* everything below only matters in live code *)
  | "define" -> define st pos arg
  | "undef" -> Hashtbl.remove st.macros arg
  | "include" ->
      let n = String.length arg in
      if n < 2 then fail pos "#include expects \"file\" or <file>";
      let quoted = arg.[0] = '"' in
      if not (quoted || arg.[0] = '<') then fail pos "#include expects \"file\" or <file>";
      let close = if quoted then '"' else '>' in
      let stop = try String.index_from arg 1 close with Not_found -> fail pos "missing terminating %c in #include" close in
      let target = String.sub arg 1 (stop - 1) in
      let path, text = find_include st pos ~quoted ~from_dir target in
      process_file st ~depth:(depth + 1) ~file:path text;
      (* Tell the lexer which line of this file comes next. *)
      Buffer.add_string st.out (Printf.sprintf "# %d \"%s\"\n" (pos.line + 1) pos.file)
  | "error" -> fail pos "#error %s" arg
  | "warning" | "pragma" | "line" | "" -> ()
  | _ -> fail pos "unknown directive #%s" name

(* Preprocess [text], the contents of [filename]. *)
let run ~include_dirs ~filename text =
  let st = { macros = Hashtbl.create 32; include_dirs; out = Buffer.create 4096 } in
  Hashtbl.replace st.macros "__STDC__" (Object "1");
  Hashtbl.replace st.macros "__APPLE__" (Object "1");
  Hashtbl.replace st.macros "__aarch64__" (Object "1");
  Hashtbl.replace st.macros "__MALACCA__" (Object "1");
  process_file st ~depth:0 ~file:filename text;
  Buffer.contents st.out
