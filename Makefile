all:
	cabal configure --enable-test
	cabal build

filetest:
	./test

test: filetest
	cabal test
