# Lexer and parser

## The lexer: `lib/lex.mll`

`ocamllex` turns this file into an OCaml module with one function per `rule`.
A rule is a list of *regular expression → action* pairs; the longest match
wins, and on a tie the first rule wins. That is why `"<<="` beats `"<<"` and
why keywords are handled by looking the identifier up in a table
(`lex_identifier`) instead of one rule per keyword.

```ocaml
rule next_token = parse
  | whitespace+ { next_token lexbuf }      (* skip, then try again *)
  | alpha alnum* as text { lex_identifier text }
  | "<<=" { SHIFTL_EQ }
```

Things worth knowing:

* **Escapes are decoded here.** `"a\n"` becomes a `STRING` token holding the
  two real bytes `a` and newline; `'\x41'` becomes `CHAR 'A'`. Later stages
  never see backslashes. (`lex_escape`.)
* **Integer literals**: decimal, `0x` hex, `0` octal, suffixes (`L`, `u`) are
  accepted and dropped. Values too big for OCaml's 63-bit int are an error.
* **Comments** are skipped here as well (the preprocessor already removed them,
  but the lexer works on its own too).
* **Line markers** from the preprocessor (`# 12 "file.h"`) update the position.
* **Positions**: `Lexing` tracks line and column in `lexbuf.lex_curr_p`. The
  rules that consume a newline call `skip_line` to bump the line count.
  `Compile.position` formats `file:line:col` for error messages.

## The parser: `lib/parse.mly`

`ocamlyacc` is the OCaml sibling of `yacc`/`bison`. The file has three parts:

```
%{ OCaml helper code %}
%token ...  %left ...  %start main   <- declarations
%%
rules:   nonterminal:  | symbols { OCaml code building a value }
```

Inside an action, `$1`, `$2`... are the values of the symbols in the rule (just
like yacc). Each rule *builds an `Ast` value* directly, so the result of
parsing is the tree.

### Grammar layout

```
main               a list of external_declaration
external_declaration   function definition | prototype | global variables
base_type, stars       the pieces of a type (see below)
declaration            "int *a, b[3] = {...};"  (shared by globals and locals)
statement              if/while/for/return/blocks/expressions
expr                   binary operators, assignment, ?:
cast_expr / unary_expr / postfix_expr / primary_expr    the tighter levels
```

### Operator precedence

Binary operators use one rule per operator plus yacc's precedence declarations:

```
%left PLUS MINUS
%left STAR SLASH MOD     <- later lines bind tighter
```

`a + b * c` then parses as `a + (b * c)`. The *prefix and postfix* levels are
spelled out as separate nonterminals instead, mirroring the C standard:

```
expr  →  cast_expr  →  unary_expr  →  postfix_expr  →  primary_expr
                        (-x !x *p &x ++x sizeof)  (f(x) a[i] x++)  (literals, names, (e))
```

This is what makes `-(char)x`, `*p++` and `sizeof (int)` parse the way C says.

### Declarations

C's declaration syntax is the trickiest part. `int *a, b;` declares a pointer
`a` and an int `b`: the stars belong to each name, not to the type. So:

* `base_type` parses `const int`, `long`, ... (`const` is accepted and ignored);
* `stars` parses `* * const` and returns **a function** that wraps a type in the
  right number of `Pointer`s (OCaml functions are ordinary values, so a grammar
  action can return one);
* `init_declarator` returns *a function from the base type to a
  `var_declaration`*; `declaration` applies it to each declarator.

`[2][3]` becomes `Array (Array (t, 3), 2)` via `array_of`. `[]` yields size 0,
which `Gen.complete_array_type` replaces by the initializer's length.

### Desugaring in the parser

To keep the generator small, the parser rewrites some syntax:

* `a[i]` → `*(a + i)`, exactly as the C standard defines it;
* `+x` → `x`;
* adjacent string literals are joined;
* a lone `(void)` parameter list → no parameters;
* array parameters (`int a[]`, `char *argv[]`) → pointers.

### Conflicts

`ocamlyacc` prints something like "1 shift/reduce conflict" when a grammar is
ambiguous. This grammar currently has **none**; keep it that way, because
conflicts are resolved silently by guessing. To investigate one, run
`ocamlyacc -v lib/parse.mly` in a scratch directory and read `parse.output`.

The classic *dangling else* (`if (a) if (b) x; else y;`) is resolved on purpose
by giving the rule without `else` a lower precedence (`%nonassoc LOWER_THAN_ELSE`)
than the `else` token, so `else` attaches to the nearest `if`.

### Errors

ocamlyacc's only error is "syntax error" (`Parsing.Parse_error`), reported with
the position of the offending token. `Compile.parse` turns it into
`file.c:12:7: error: syntax error`.

### Source positions in the tree

Statements are wrapped in `Located (loc, stmt)` and declarations carry a `loc`
field, filled in by `here ()` in `parse.mly`. `Gen` remembers the latest one it
passed so that *its* errors can also name a file and line.

## Adding a feature: a worked example

Suppose we add the `%` operator... which already exists, so here is how it was
done, as a checklist for anything similar:

1. **Token**: `lex.mll` has `| "%" { MOD }`, and `parse.mly` declares `%token MOD`.
2. **Precedence**: `%left STAR SLASH MOD` gives it the right level.
3. **Rule**: `| expr MOD expr { BinaryExpr (Mod, $1, $3) }`.
4. **AST**: `Mod` is a constructor of `Ast.operator`.
5. **Generator**: `Gen.emit_binop` has a case for `Mod`. Because OCaml checks
   `match` exhaustiveness and the build treats that warning as an error, step 4
   makes the compiler point at step 5 if you forget it.
