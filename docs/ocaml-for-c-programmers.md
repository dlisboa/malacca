# OCaml for C programmers

This is not an OCaml tutorial. It covers just the features this code base uses,
each anchored to something in C and to a place in the source where you can see
it. If you want more, the free book *Real World OCaml* (dev.realworldocaml.org)
and the manual at ocaml.org are good.

## The big differences, in one paragraph

OCaml is statically typed with full type inference (you rarely write types, the
compiler works them out and checks them), values are **immutable by default**,
everything is an **expression** that has a value (there is no statement/expression
split: `if` and `match` return values), and memory is garbage-collected. Data
is described with **variants** and taken apart with **pattern matching**, which
is what most of this compiler is made of.

## Bindings, not variables

```ocaml
let x = 5            (* like: const int x = 5; *)
let y = x + 1 in     (* "in" starts the expression where y is visible *)
y * 2
```

`let name = value` at the top of a file defines a name for the whole file.
`let ... in ...` defines a name for the expression after `in`. Neither can be
reassigned. When something genuinely must change, we use a **ref**:

```ocaml
let counter = ref 0      (* like: int counter = 0; but boxed *)
counter := !counter + 1  (* write: := , read: ! *)
```

`!counter` reads, `counter := v` writes. You will see this in `gen.ml` for
state such as `label_counter`, `frame_size` and `current_function`. (It is
global mutable state, which is not elegant, but a compiler that runs once per
process and generates code in order is a reasonable place for it.)

## Functions

```ocaml
let add a b = a + b          (* int add(int a, int b) { return a + b; } *)
let five = add 2 3           (* no parentheses or commas to call *)
let inc = add 1              (* partial application: a function waiting for b *)
```

The last value computed is the return value; there is no `return`. A function
whose result is not interesting returns `()`, the "unit" value, like `void`.
Sequencing is with `;`: `emit "ret"; current_function := ""`.

**Labelled arguments** are written `~name:value`, as in
`gen_loop_body ~break_l:end_l ~continue_l:top body`. They exist only to make
call sites self-explanatory.

**`fun`** makes an anonymous function: `List.map (fun x -> x + 1) [1; 2; 3]`.

**`rec`**: a function that calls itself (or its siblings, joined with `and`)
must be declared `let rec`. `gen_expr`, `gen_binary`, `gen_addr` and `gen_call`
are one such group: they are mutually recursive, like functions that all have
forward declarations in C.

## Variants: tagged unions

This is OCaml's most important feature, and it is how the syntax tree is
written (`lib/ast.ml`):

```ocaml
type type_spec =
  | TypeVoid
  | TypeInt
  | Pointer of type_spec          (* carries one value *)
  | Array of type_spec * int      (* carries two *)
```

The C equivalent is a `struct` with a `tag` enum and a `union` of payloads, and
you have to remember to check the tag yourself. In OCaml the compiler knows the
tag and checks you handled every case.

`Pointer TypeInt` builds the value for `int *`; `Pointer (Pointer TypeChar)`
is `char **`. Records are the equivalent of structs: `{ file : string; line : int }`.

## Pattern matching

`match` takes a value apart. It is like `switch` but it can look inside:

```ocaml
let rec sizeof = function
  | TypeVoid | TypeChar -> 1          (* | combines cases *)
  | TypeInt -> 4
  | TypeLong | Pointer _ -> 8         (* _ means "don't care what it carries" *)
  | Array (t, n) -> n * sizeof t      (* binds t and n from inside the value *)
```

`function` is shorthand for "a function that immediately matches its argument".
Patterns nest and can have guards:

```ocaml
match (op, lty, rty) with
| Add, Pointer t, r when is_integer r -> ...   (* pointer + integer *)
| Sub, Pointer t, Pointer _ -> ...             (* pointer - pointer *)
| _ -> ...                                     (* everything else *)
```

If you forget a case the compiler warns, and the project's build settings turn
that warning into an error. This is the main reason adding a new feature to the
AST is safe: the compiler lists every place that must learn about it.

## Option: a value that might be missing

```ocaml
type 'a option = None | Some of 'a
```

Where C would use a null pointer or a sentinel, OCaml uses `option`. In the AST
`init : expression option` means "the initializer, if there is one". You must
match on it, so you cannot forget the "no initializer" case:

```ocaml
match init with
| None -> ()
| Some e -> ...
```

`Hashtbl.find_opt table key` returns an `option` too.

## Lists and tuples

```ocaml
let xs = [1; 2; 3]       (* singly linked list; elements separated by ; *)
let ys = 0 :: xs         (* cons: [0; 1; 2; 3] *)
let zs = xs @ [4]        (* append *)
let pair = (1, "one")    (* tuple: an anonymous struct *)
let (n, s) = pair        (* take it apart *)
```

Lists are used everywhere for sequences of statements and arguments. Handy
functions: `List.map f xs`, `List.iter f xs` (run `f` on each, for effect),
`List.iteri` (also passes the index), `List.length`, `List.find_opt`.
`List.iter2`/`List.map2` walk two lists together.

## Strings and formatting

Strings are immutable. `^` concatenates: `"_" ^ name`. For building text
we use printf-style formatting, much like C:

```ocaml
Printf.sprintf "%s:%d" file line          (* returns a string *)
emit "mov x0, #%d" 5                       (* our own printf-like helper *)
```

`emit` and `fail` in `gen.ml` are built with `Printf.ksprintf`, which formats
and then passes the resulting string to a function of yours (here: append to
the output buffer, or raise an error). `%s` is a string, `%d` an int, `%Ld` a
64-bit int, `%S` a string with quotes and escapes.

`Buffer.t` is a growable string you append to: the right tool for building
assembly output.

## Exceptions

```ocaml
exception Error of string
raise (Error "boom")
try Parse.main ... with Parsing.Parse_error -> ...
```

Used for compile errors: `Gen.fail` raises `Gen.Error`, `Compile` catches and
re-raises as `Compile.Error`, and `bin/main.ml` prints it and exits with 1.

## Modules: one file is one module

`lib/gen.ml` is the module `Gen`; from another file you call `Gen.program`.
`open Ast` at the top of a file lets you write `Add` instead of `Ast.Add`.
There are no header files: what a module exposes is whatever it defines
(an optional `.mli` file would narrow that; this project has none).

`dune` is the build system (think `make` plus automatic dependency discovery).
`lib/dune` describes the library, `bin/dune` the executable.

## `[@@deriving show]`

Lines like `type expression = ... [@@deriving show]` ask a build-time plugin
to generate `show_expression`, which prints a value as text. That is what
powers `malacca --dump-ast` and gives you printing of the syntax tree without
writing any code.

## `ocamllex` and `ocamlyacc` files

`lex.mll` and `parse.mly` are not OCaml: they are input to code generators
(like `lex` and `yacc` in C), which `dune` runs automatically and which produce
OCaml. The curly-brace parts are OCaml. See
[lexer-and-parser.md](lexer-and-parser.md).

## Reading tips

* A `|` at the start of a line begins a case. Indentation is not significant,
  so look at the `|` characters to find the cases of a `match`.
* A `match` inside another `match` arm needs parentheses or `begin ... end`,
  otherwise the inner one swallows the following cases. You will see
  `(match ... )` in many places for this reason.
* `ignore (gen_expr e)` means "evaluate this for its effect and drop the
  result". OCaml warns when you silently discard a non-unit value.
* `function A -> ... | B -> ...` is `fun x -> match x with A -> ... | B -> ...`.
* Types are inferred, so hovering in an editor with the OCaml LSP (the VS Code
  "OCaml Platform" extension) is the quickest way to learn what something is.
