TARGETS = haskalah-test haskalah-test.byte haskalah haskalah.byte
OCAML_TARGETS = src/main.native test/test_main.native
OCAML.byte_TARGETS = src/main.byte test/test_main.byte
OCAMLBUILD_FLAGS = -use-ocamlfind -classic-display -no-links
OCAMLBUILD_FLAGS += -Is src,test -cflags -strict-sequence,'-w +a-4-44',-g
OCAMLBUILD_FLAGS += -lflag -g
OCAMLBUILD_FLAGS += -tags 'package(batteries)','package(oUnit)'

DEBUG_FLAGS = `ocamlfind query -recursive -i-format batteries` -I src -I test

.PHONY: all clean debug test-debug
all: $(TARGETS)

debug: all
	rlwrap ocamldebug $(DEBUG_FLAGS) ./haskalah.byte

test-debug: all
	rlwrap ocamldebug $(DEBUG_FLAGS) ./haskalah-test.byte

# Link the targets into a nicer place
haskalah: src/main.native
	ln -sf _build/$< $@
haskalah-test: test/test_main.native
	ln -sf _build/$< $@
haskalah.byte: src/main.byte
	ln -sf _build/$< $@
haskalah-test.byte: test/test_main.byte
	ln -sf _build/$< $@

# Make ocamlbuild do all the actual dependency generation / checks etc
$(OCAML_TARGETS) $(OCAML.byte_TARGETS):
	ocamlbuild $(OCAMLBUILD_FLAGS) $@

clean:
	$(RM) $(TARGETS)
	ocamlbuild -clean -classic-display
