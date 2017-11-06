OCB_FLAGS := \
	-tag 'color(always)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-ocamlfind -pkgs 'str,ppx_deriving.std' \
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

test: main
	./main.byte

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: test main clean $(mlis)
