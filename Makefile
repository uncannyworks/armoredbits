.PHONY: build
build:
	nix-shell --run "cabal configure && cabal build"

.PHONY: run-server
run-server:
	nix-shell --run "cabal run armoredbits-server"

.PHONY: run-client
run-client:
	nix-shell --run "cabal run armoredbits-client"

.PHONY: clean
clean:
	rm -rf dist

.PHONY: deps
deps:
	cabal2nix . > packages.nix
	
.PHONY: docs
docs:
	nix-shell --run "cabal haddock"
