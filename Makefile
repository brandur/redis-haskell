all : redis

redis : 
	cabal configure -ftesting -fhacking
	cabal build

benchmarks :
	cd testsuite; make benchmarks

tests :
	cd testsuite; make

clean :
	rm -rf dist
	cd testsuite; make clean

