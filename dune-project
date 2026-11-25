(lang dune 3.17)

(name malacca)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Diogo Lisboa")

(maintainers "Diogo Lisboa")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name malacca)
 (synopsis "A C compiler for ARM64 macOS")
 (description "A C compiler written in OCaml. It preprocesses, parses and compiles a subset of C to ARM64 assembly, then assembles and links it with the system tools.")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
