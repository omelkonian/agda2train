.PHONY: repl build install installTest test golden cleanTest allTests stdlib unimath typetopology

default: repl

repl:
	cabal repl agda2train # e.g. `:set args -r -o json -itest test/First.agda ... main ... :r ... main`

replDB:
	cabal repl agda2train-db

build:
	cabal build

install:
	cabal install --overwrite-policy=always

# Testing
installTest:
	cabal install --overwrite-policy=always --installdir=test --install-method=copy

golden: installTest
	make -C test all
	make -C test golden

test: installTest
	make -C test

cleanTest:
	@rm test/agda2train
	make -C test clean
	make -C test cleanGolden

# DB example

testDB:
	cabal run agda2train-db -- add data/db.json test/golden/Test.PiVsFun.json
	cabal run agda2train-db -- query data/db.json data/query.json \
		| tail -n1 | cmp data/expected_response.txt

# Extracting training data from whole libraries

STDLIB?=$(HOME)/git/agda-stdlib
UNIMATH?=$(HOME)/git/agda-unimath
TYPETOP?=$(HOME)/git/TypeTopology

allTests:
	cabal run agda2train -- -r -v agda2train:10 \
	  -ojson/ -itest/ test/All.agda
	zip -j data/allTests.zip json/*.json

stdlib:
	cabal run agda2train -- -r -v agda2train:10 \
		-o$(STDLIB)/json/ -i $(STDLIB) -i $(STDLIB)/src/ $(STDLIB)/Everything.agda
	zip -j data/stdlib.zip $(STDLIB)/*.json

unimath:
	cabal run agda2train -- -r -v agda2train:10 \
		-o$(UNIMATH)/json/ -i $(UNIMATH)/src/ $(UNIMATH)/src/everything.lagda.md
	zip -j data/unimath.zip $(UNIMATH)/*.json

typetopology:
	cabal run agda2train -- -r -v agda2train:10 \
		-o$(TYPETOP)/json/ -i $(TYPETOP)/source/ $(TYPETOP)/source/index.lagda
	zip -j data/typetopology.zip $(TYPETOP)/*.json

	  # +RTS -H3G -M3G -RTS \
   # --no-privates \

testInfer:
	cabal run agda2train -- -r -v agda2train:10 \
	  -oinfer-test/json/ -i infer-test/ infer-test/All.agda
