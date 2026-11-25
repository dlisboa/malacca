# The preprocessor

`lib/preproc.ml`. It is a pure text-to-text pass: the lexer and parser never
see a `#include` or a macro. `malacca -E prog.c` prints its output.

## What it supports

| Feature | Notes |
| --- | --- |
| `#include "file"` | searched in the including file's directory, then `-I` directories, then the built-in headers |
| `#include <file>` | `-I` directories, then the built-in headers |
| `#define NAME body` | object-like macro |
| `#define NAME(a, b) body` | function-like macro; a call needs `(` after the name and must fit on one line |
| `#undef NAME` | |
| `#if`, `#elif`, `#else`, `#endif`, `#ifdef`, `#ifndef` | `#if` understands integers, `defined(X)`, `defined X`, `! ~ - +`, arithmetic, comparisons, `&& \|\|`, `?:` |
| `#error text` | stops compilation with that message |
| `#pragma`, `#line`, `#warning` | accepted and ignored |
| `__LINE__`, `__FILE__`, `__STDC__`, `__APPLE__`, `__aarch64__` | predefined |
| `\` at the end of a line | joins lines (long macros) |

Not supported: `#` (stringify) and `##` (paste) inside macro bodies, variadic
macros, macro arguments spread over several lines, `#include MACRO`,
`#pragma once` (use include guards), `#include_next`.

## How it works

1. **Comments are removed** first (`strip_comments`), skipping over string and
   character literals so that `"http://x"` survives. A block comment becomes a
   space plus as many newlines as it had, so line numbers do not move.
2. The text is split into lines; lines ending in `\` are joined.
3. For each logical line:
   * if it starts with `#`, it is a **directive** (`directive_line`);
   * otherwise, if the current `#if` region is active, its macros are expanded
     (`expand`) and it is copied to the output;
   * otherwise it becomes an empty line.

Conditionals are tracked with a stack of records (`cond`): is the enclosing
region live, has some branch already been taken, is this branch live. That is
enough to get `#elif`/`#else` right and to skip nested `#if`s inside dead code
without evaluating them.

### Macro expansion

`expand` scans the line looking for identifiers (never inside string/char
literals, and never inside numbers like `0x1F`).

* An **object-like** macro is replaced by its body, and the body is scanned
  again (it may use other macros).
* A **function-like** macro is only expanded if the next non-blank character is
  `(`. The arguments are split at top-level commas (so `F(a, g(b, c))` has two),
  expanded themselves, substituted for the parameter names in the body, and the
  result is scanned again.
* The list `hidden` holds the macros currently being expanded. A macro's own
  name inside its expansion is left alone; that is what stops
  `#define X X + 1` from recursing forever.

### Line numbers and the lexer

Error messages must say `stdio.h:12`, not "line 400 of the merged text", so the
output keeps its shape:

* every input line yields exactly one output line (empty for directives and
  skipped code);
* around an `#include` the preprocessor writes **line markers**:

  ```
  # 1 "stdio.h"
  ...contents of stdio.h...
  # 6 "main.c"
  ```

  meaning "the next line is line N of that file". `#` is not otherwise a C
  token, so `lex.mll` has one rule that recognises exactly this shape and
  updates the lexer's position. (Real compilers use the same convention.)

## The built-in headers

The real macOS headers (`/Applications/Xcode.app/.../usr/include/stdio.h`) are
full of things this compiler cannot parse yet: `typedef`, `struct`, `__attribute__`,
`_Nullable` annotations, function pointers... So malacca ships its own small
headers in `include/`: `stdio.h`, `stdlib.h`, `string.h`, `ctype.h`, `limits.h`,
`stddef.h`, `stdbool.h`. They only *declare* functions; the code comes from the
system's libc (`libSystem`) at link time, exactly as with a real compiler.

They are embedded in the compiler's executable, so there is nothing to install
and no search path to get wrong:

1. `dune` runs `tools/embed_headers.ml` on `include/*.h` (the rule in
   `lib/dune`);
2. that tool writes `builtin_headers.ml`, an OCaml module holding each file's
   text as a string;
3. `Preproc.find_include` falls back to `Builtin_headers.find` when no file is
   found on disk.

Since there are no typedefs yet, a few types are macros: `size_t` is `long`,
`bool` is `int`, `true`/`false` are `1`/`0`, and `NULL` is `((void *)0)`.

To add a function, add its prototype to the right header in `include/` and
rebuild. Prototypes are required: calling an undeclared function is an error.
