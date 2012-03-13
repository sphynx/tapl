all:	test

clean:
	rm -vf *.hi *.o
	rm -f Interpreter
	rm -f Tests

compile:
	ghc -Wall Interpreter.hs -main-is Interpreter.main

run:	compile
	./Interpreter

test:	compile
	ghc -package test-framework -package test-framework-hunit \
		-threaded Tests.hs -o Tests \
		-main-is Tests.main
	./Tests --maximum-generated-tests=5000 +RTS -N2

# exclude LambdaEncoding from hlinting, since we have lots of
# intended redundand lambdas there
hlint:
	find -name '*.hs' -not -name 'LambdaEncoding.hs' | xargs hlint
