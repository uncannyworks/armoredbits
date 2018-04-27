.PHONY: build
build:
	nix-shell --run "cabal configure && cabal build"

.PHONY: clean
clean:
	rm -rf dist

.PHONY: deps
deps:
	cabal2nix . > packages.nix
	
.PHONY: docs
docs:
	nix-shell --run "cabal haddock"
