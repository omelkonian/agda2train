.PHONY : repl build install

default: repl

repl :
	cabal repl # e.g. `:set args -r -o json -itest test/First.agda ... main ... :r ... main`

build :
	cabal build

install :
	cabal install --overwrite-policy=always
