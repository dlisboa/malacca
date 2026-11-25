# malacca documentation

malacca is a C compiler written in OCaml that produces executables for
**ARM64 macOS** (Apple silicon). Read these in order if you are new to the
code base:

| Document | What it explains |
| --- | --- |
| [overview.md](overview.md) | The compilation pipeline, the source tree, how to build and run. Start here. |
| [ocaml-for-c-programmers.md](ocaml-for-c-programmers.md) | The parts of OCaml this code uses, explained with C analogies. |
| [preprocessor.md](preprocessor.md) | `#include`, `#define`, `#if`; the built-in headers. |
| [lexer-and-parser.md](lexer-and-parser.md) | Turning text into a syntax tree with `ocamllex` and `ocamlyacc`. |
| [codegen.md](codegen.md) | Turning the tree into ARM64 assembly: the stack-machine design, the calling convention, stack frames. |
| [testing.md](testing.md) | The end-to-end test suite and how to add a test. |
| [limitations.md](limitations.md) | What is not supported yet, and ideas for what to do next. |
