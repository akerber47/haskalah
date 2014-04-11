
A Haskell compiler.

# Installation Instructions #

The implementation language is OCaml (Batteries Included). We use OUnit2
for unit testing. I haven't written an install script, but
installing ocaml, opam, and then batteries and ounit2 through opam should
do the trick. Once those are installed, just run

	make

to get the compiler running. The default makefile builds both bytecode
and native code versions of the compiler and its test suite.

We use a custom-built canonical LR(1) parser generator, because
resolving grammar conflicts is too hard to be left to confusing yacc
output. The parser can be rebuilt by running

	make parse-gen

Finally, if you want to run the compiler with the ocaml debugger, run

	make debug

or

	make test-debug

for the test suite. Note that the ocaml debugger only works on bytecode
executables, not native code.

# Implementation Details #

The source language is Haskell 98, as specified in the Revised Report
* http://www.haskell.org/onlinereport/
It includes some features (fixity resolution, better line comments,
pattern guards, no n+k patterns, etc) from Haskell 2010. See
* http://www.haskell.org/onlinereport/haskell2010/
* http://www.haskell.org/haskellwiki/Haskell_2010
for details of these changes to the report.

Some features of Haskell 98/10 are not supported. In particular, we do not
include a full standard library, and do not support unicode.

We do not support any of the fancy modes of a full-featured Haskell compiler:
no `--interactive`, no `--make`, no `-e`.

We compile to LLVM, not native code.

See additional ideas in
* http://www.cs.tufts.edu/~nr/comp150fp/ss.html
* http://prog21.dadgum.com/30.html
* http://compilers.iecc.com/crenshaw/
* http://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf
