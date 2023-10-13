.PHONY: repl build install installTest test golden cleanTest stdlib unimath typetopology

default: repl

repl:
	cabal repl # e.g. `:set args -r -o json -itest test/First.agda ... main ... :r ... main`

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

# Extracting training data from whole libraries

STDLIB?=$(HOME)/git/agda-stdlib
UNIMATH?=$(HOME)/git/agda-unimath
TYPETOP?=$(HOME)/git/TypeTopology

stdlib:
	cabal run agda2train -- -r -v agda2train:10 \
		-ojson/stdlib/ -i $(STDLIB) $(STDLIB)/Everything.agda

unimath:
	cabal run agda2train -- -r -v agda2train:10 \
		-ojson/unimath/ -i $(UNIMATH)/src/ $(UNIMATH)/src/everything.lagda.md

typetopology:
	cabal run agda2train -- -r -v agda2train:10 \
		-ojson/typetopology -i $(TYPETOP)/source/ $(TYPETOP)/source/index.lagda

