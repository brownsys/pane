all:
	cabal configure --enable-test
	cabal build

filetest:
	./test

test: filetest
	cabal test

clientlibs:
	cd client-libs; mvn package; mvn install

clean:
	cabal clean
