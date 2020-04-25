all: src/Interpreter

grammar: Grammar.cf
	rm -f build/*
	mkdir -p build
	(cd build \
		&& bnfc --functor ../Grammar.cf -m \
		&& make \
		&& happy -gca --info=bad.txt ParGrammar.y)

src/Interpreter: src/*.hs
	ghc src/Interpreter.hs \
		-Wall -Wincomplete-patterns\
		-package mtl \
		-package vector \
		-i build/AbsGrammar \
		-i build/LexGrammar \
		-i build/ParGrammar \
		-i build/ErrM \
		-i src/TypeChecker.hs \
		-i src/CommonDeclarations.hs \
		-i src/Expressions.hs \
		-i src/Statements.hs \
		-o build/Interpreter

clean:
	rm src/*.hi
	rm src/*.o
