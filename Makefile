TARGETS = haskalah-test haskalah
OCAML_TARGETS = src/main.native test/main.native
OCAMLBUILD_FLAGS = -use-ocamlfind -classic-display
OCAMLBUILD_FLAGS += -Is src,test -cflags -strict-sequence,-w+a-4-44,-g
OCAMLBUILD_FLAGS += -libs batteries,oUnit

.PHONY: all clean
all: $(TARGETS)

# Copy the targets into a nicer place
haskalah: src/main.native
	cp -f --preserve=links $@ $<
haskalah-test: test/main.native
	cp -f --preserve=links $@ $<

# Make ocamlbuild do all the actual dependency generation etc
$(OCAML_TARGETS):
	ocamlbuild $(OCAMLBUILD_FLAGS) $@

clean:
	$(RM) $(TARGETS)
	ocamlbuild -clean -classic-display
