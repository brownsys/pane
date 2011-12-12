all:
	cabal configure
	cabal build

filetest:
	./dnp -f tests/ex1.dnp
	./dnp -f tests/ex2.dnp
	./dnp -f tests/ex3.dnp
	./dnp -f tests/ex4.dnp
	./dnp -f tests/ex5.dnp
