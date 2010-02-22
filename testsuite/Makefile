hss := $(shell find ../ -name "*.hs" -print)

all : tests

tests : $(hss)
	mkdir -p dist/
	ghc -Wall --make -i../src/:./tests/ -hidir ./dist/ -odir ./dist/ -o ./dist/tests -hide-all-packages -package base -package convertible-text -package failure -package HUnit -package network -package text -package transformers -XOverloadedStrings ./tests/Test/Tests.hs
	dist/tests

benchmarks : $(hss)
	mkdir -p dist/
	ghc -Wall --make -i../src/:./tests/ -hidir ./dist/ -odir ./dist/ -o ./dist/tests -hide-all-packages -package base -package convertible-text -package failure -package HUnit -package network -package text -package transformers -package time -XOverloadedStrings ./benchmarks/Performance.hs
	./dist/benchmarks

clean :
	rm -rf dist/
	rm -rf *.prof

