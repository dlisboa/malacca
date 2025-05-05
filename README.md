# malacca

A C Compiler in OCaml.

This project is part of my sabbatical.

## Objectives

The idea is to be able to compile simple programs and have them run on ARM64
macOS. No other platform is to be supported. In time expand the set of programs
that it compiles, focusing on completeness with the C11 standard, not speed.

## How far along is it?

It's able to parse some programs in C. Extensibility is pretty easy with the
current setup with `ocamllex` and `ocamlyacc` so extending the range of programs
is not complicated.

Code generation is in early stages.
