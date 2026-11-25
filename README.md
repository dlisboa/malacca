# malacca

A C compiler in OCaml.

This project is part of my sabbatical.

## Objectives

The idea is to be able to compile simple programs and have them run on ARM64
macOS. No other platform is to be supported. In time expand the set of programs
that it compiles, focusing on completeness with the C11 standard, not speed.

## Quick start

You need OCaml with `dune`, and the Xcode command line tools (for the system
assembler and linker).

```sh
dune build
cat > hello.c <<'EOT'
#include <stdio.h>

int main(void)
{
    printf("Hello, world!\n");
    return 0;
}
EOT
dune exec malacca -- hello.c
./hello
```

`dune test` runs the test suite. `make` has shortcuts for these.

## How far along is it?

It compiles a useful subset of C: `char`/`int`/`long`, pointers, arrays,
functions (including calling `printf` and friends), globals, all the usual
operators and control flow, and a preprocessor with `#include`/`#define`/`#if`.
Not yet: `struct`, `typedef`, `enum`, `switch`, floating point, `unsigned`.
See [docs/limitations.md](docs/limitations.md).

## Documentation

Everything is explained in [docs/](docs/README.md): the pipeline, a primer on
the OCaml used here (written for C programmers), the parser, the code
generator and how to add tests.
