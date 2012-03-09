.PHONY: all
all:
	cabal build

.PHONY: configure
configure:
	cabal configure

.PHONY: clean
clean:
	cabal clean
