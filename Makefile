all: Grammar.cf
	mkdir -p build
	rm -f build/*
	(cd build && bnfc ../Grammar.cf -m && make && happy -gca --info=bad.txt ParGrammar.y)
