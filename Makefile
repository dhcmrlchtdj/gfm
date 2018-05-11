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
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

test: main
	# ./vfmd-test/run_tests --actual-fails --expected-fails --dir './vfmd-test/tests/**/*' './main.byte -'
	# ./vfmd-test/run_tests --dir './vfmd-test/tests/**/*' './main.byte -'
	# echo '# header\nThe **`ls` command** [_lists_ files](/ls-cmd).' | ./main.byte -
	echo 'The **`ls` command** [_lists_ files](/ls-cmd).' | ./main.byte -

$(mlis):
	-@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis) jsoo test
