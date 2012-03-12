all:	test

clean:
	rm -vf *.hi *.o
	rm -f Interpreter

compile:	clean
	ghc Interpreter.hs -main-is Interpreter.main

test:	compile
	./Interpreter

hlint:
	hlint .
