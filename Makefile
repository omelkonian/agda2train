.PHONY : repl build install stdlib unimath typetopology test

default: repl

repl :
	cabal repl # e.g. `:set args -r -o json -itest test/First.agda ... main ... :r ... main`

build :
	cabal build

install :
	cabal install --overwrite-policy=always

stdlib :
	cabal run agda2train -- -r -v agda2train:10 -ojson/ -i /home/omelkonian/git/agda-stdlib/ /home/omelkonian/git/agda-stdlib/Everything.agda

unimath :
	cabal run agda2train -- -r -v agda2train:10 -ojson/ -i /home/omelkonian/git/agda-unimath/src/ /home/omelkonian/git/agda-unimath/src/everything.lagda.md

typetopology :
	cabal run agda2train -- -r -v agda2train:10 -ojson/ -i /home/omelkonian/git/TypeTopology/source/ /home/omelkonian/git/TypeTopology/source/index.lagda

test :
	cabal install --overwrite-policy=always --installdir=test --install-method=copy
	make -C test

golden :
	make -C test golden
