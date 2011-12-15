all:
	cabal configure
	cabal build

filetest:
	./test
