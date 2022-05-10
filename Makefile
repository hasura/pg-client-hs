.PHONY: all
all:

.PHONY: format
format:
	cabal-fmt -i pg-client.cabal
	find src test bench -name '*.hs' | xargs ormolu -ie

PROJECT ?= cabal.project
CABAL = cabal --project=$(PROJECT)

.PHONY: freeze
freeze:
	$(CABAL) freeze \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: configure
configure:
	$(CABAL) configure \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: update
update:
	$(CABAL) update

.PHONY: build-deps
build-deps:
	$(CABAL) build \
	  --only-dependencies \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: build
build:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  pg-client

.PHONY: build-all
build-all:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: test-all
test-all:
	$(CABAL) test \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: ghcid
ghcid:
	ghcid --command "\
	  $(CABAL) repl \
	    --repl-option='-fobject-code' \
	    --repl-option='-O0' \
	    pg-client \
	  "

.PHONY: ghcid-test
ghcid-test:
	ghcid \
	  --command "\
	    $(CABAL) repl \
	      --repl-option '-fobject-code' \
	      --repl-option '-O0' \
	      pg-client-test \
	    " \
	--test ":main"
