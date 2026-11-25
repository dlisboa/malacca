# Thin wrappers around dune; the real build description is in the dune files.

.PHONY: build test clean

build:
	dune build

test:
	dune test

# Compile and run any C file:  make run FILE=test/programs/hello.c
run: build
	_build/default/bin/main.exe -o /tmp/malacca-run $(FILE) && /tmp/malacca-run

clean:
	dune clean
