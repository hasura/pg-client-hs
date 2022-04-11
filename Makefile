.PHONY: all
all:

.PHONY: format
format:
	cabal-fmt -i pg-client.cabal
	find src test bench -name '*.hs' | xargs ormolu -ie

PROJECT = "cabal.project"

.PHONY: freeze
freeze:
	cabal freeze \
	  --project=$(PROJECT) \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: configure
configure:
	cabal configure \
	  --project=$(PROJECT) \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: update
update:
	cabal update --project=$(PROJECT)

.PHONY: build-deps
build-deps:
	cabal build \
	  --project=$(PROJECT) \
	  --only-dependencies \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: build-all
build-all:
	cabal build \
	  --project=$(PROJECT) \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: test-all
test-all:
	cabal test \
	  --project=$(PROJECT) \
	  --enable-tests \
	  --enable-benchmarks \
	  all
