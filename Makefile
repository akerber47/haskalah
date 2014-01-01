TARGETS = haskalah-test haskalah
OCAML_TARGETS = src/main.native test/main.native
OCAMLBUILD_FLAGS = -use-ocamlfind -classic-display -no-links
OCAMLBUILD_FLAGS += -Is src,test -cflags -strict-sequence,'-w +a-4-44',-g
OCAMLBUILD_FLAGS += -tags 'package(batteries)','package(oUnit)'

.PHONY: all clean $(OCAML_TARGETS)
all: $(TARGETS)

# Link the targets into a nicer place
haskalah: src/main.native
	ln -sf _build/$< $@
haskalah-test: test/main.native
	ln -sf _build/$< $@

# Make ocamlbuild do all the actual dependency generation / checks etc
$(OCAML_TARGETS):
	ocamlbuild $(OCAMLBUILD_FLAGS) $@

clean:
	$(RM) $(TARGETS)
	ocamlbuild -clean -classic-display
