This is a compiler for a simplified Haskell-like programming language, Hsk. Hsk
is a lazy, purely functional, strongly typed language including operators
(with precedence) and pattern matching syntax. Data types are Int, String,
lists, and custom compound types.

It does NOT include list sections, sequences, or comprehensions; tuples; modules
or namespaces; type classes; or non-ASCII character set support.

Lexing and parsing use monadic combinators as in
http://www.cs.tufts.edu/~nr/comp150fp/archive/graham-hutton/monadic-parsing-jfp.pdf
Language definition and grammars based on the Haskell 98 Report:
http://www.haskell.org/onlinereport/

We use a "nanopass" framework for intermediate steps, as in
http://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf
Intermediate AST representations are built with this in mind.

We compile to LLVM, not native code.

See additional ideas in
http://www.cs.tufts.edu/~nr/comp150fp/ss.html
http://prog21.dadgum.com/30.html
http://compilers.iecc.com/crenshaw/
