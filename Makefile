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
	-pkg re.perl \
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

jsoo: main
	js_of_ocaml --opt=3 --pretty +nat.js +weak.js ./main.byte

test: main
	# cd ./vfmd-test && \
		# ./run_tests \
		# --dir './tests/block_level/atx_header/' \
		# --actual-fails --expected-fails \
		# '../main.byte -'
	echo '# header\nThe **`ls` command** [_lists_ files](/ls-cmd).' | ./main.byte -

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis) jsoo test
