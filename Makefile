.PHONY: build
build:
	nix-shell --run "cabal new-build"

.PHONY: test
test:
	nix-shell --run "cabal new-test"

.PHONY: run-server
run-server:
	nix-shell --run "cabal new-run armoredbits-server"

.PHONY: run-client
run-client:
	nix-shell --run "cabal new-run armoredbits-client"

.PHONY: clean
clean:
	rm -rf dist
	rm -rf dist-newstyle

.PHONY: deps
deps:
	cabal2nix . > packages.nix
	
.PHONY: docs
docs:
	nix-shell --run "cabal haddock"
