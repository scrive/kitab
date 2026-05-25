build: ## Build the project in fast mode
	@cabal build

install: build ## Install kitab in ~/.local/bin/
	cp -f $(EXECUTABLE) ~/.local/bin/kitab

manual: ## Generate doc/MANUAL.md from doc/base/MANUAL.md
	@ghcup --offline run --ghc 9.12.2 -- scripths --code-style remove --output-style raw -o doc/MANUAL.md doc/base/MANUAL.md

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

test-accept: ## Run the golden tests and accept new output
	@cabal run -- kitab-test --accept

lint: ## Run the code linter
	@find kitab-prelude app test/Test test/Main.hs src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code stylers
	@cabal-gild --mode=format --io=kitab.cabal
	@cabal-gild --mode=format --io=kitab-prelude/kitab-prelude.cabal
	@fourmolu -q --mode inplace kitab-prelude test/Test test/Main.hs src app

tags: ## Generate ctags for the project with `ghc-tags`
	@ghc-tags -c

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

EXECUTABLE := $(shell cabal list-bin kitab)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
