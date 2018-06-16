OCB_FLAGS := \
	-tag 'color(always)' \
	-tag safe_string \
	-tag short_paths \
	-tag strict_sequence \
	-tag keep_locs \
	-tag keep_docs \
	-tag bin_annot \
	-tag principal \
	-tag nopervasives \
	-use-ocamlfind \
	-pkg batteries \
	-pkg re \
	-pkg alcotest \
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

.PHONY: main
main: native

.PHONY: test
test:
	@$(OCB) src/test.native
	@./test.native

.PHONY: byte
byte: $(mlis)
	@$(OCB) src/main.byte
	@ln -sf ./main.byte ./main

.PHONY: native
native: $(mlis)
	@$(OCB) src/main.native
	@ln -sf ./main.native ./main

.PHONY: $(mlis)
$(mlis):
	-@$(OCB) $@.inferred.mli

.PHONY: clean
clean:
	@ocamlbuild -clean
	@rm -rf ./main

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
	ocp-indent -i src/*.ml
