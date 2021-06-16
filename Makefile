.PHONY: all
all:

.PHONY: format
format:
	find src test bench -name '*.hs' | xargs stylish-haskell -i
