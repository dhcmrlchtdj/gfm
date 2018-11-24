.PHONY: main
main: native

.PHONY: test
test:
	@ ocamlbuild -use-ocamlfind src/test.native
	@ ./test.native

.PHONY: native
native: $(mlis)
	@ ocamlbuild -use-ocamlfind src/main.native
	@ ln -sf ./main.native ./main

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))
.PHONY: $(mlis)
$(mlis):
	-@ ocamlbuild -use-ocamlfind $@.inferred.mli

.PHONY: clean
clean:
	@ ocamlbuild -clean
	@ rm -rf ./main

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
	ocp-indent -i src/*.ml
