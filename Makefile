.PHONY: all
all:

.PHONY: format
format:
	cabal-fmt -i pg-client.cabal
	find src test bench -name '*.hs' | xargs ormolu -ie
