SHELL := /bin/bash

run: build
	./_build/default/bin/bin.exe

build:
	dune build @default --profile=dev

test:
	dune runtest

clean:
	dune clean

fmt:
	ocamlformat -i */*.ml

doc:
	dune build @doc

release:
	dune build @default --profile=release

install:
	opam install .

uninstall:
	opam remove .

.PHONY: run build test clean fmt doc
.PHONY: release install uninstall
