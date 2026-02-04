build: ## Build the project in fast mode
	@cabal build

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

test-accept: ## Run the golden tests and accept new output
	@cabal run -- kitab-test -p '/golden/' --accept

lint: ## Run the code linter
	@find kitab-prelude app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code stylers
	@cabal-gild --mode=format --io=kitab.cabal
	@cabal-gild --mode=format --io=kitab-prelude/kitab-prelude.cabal
	@fourmolu -q --mode inplace kitab-prelude test src app

tags: ## Generate ctags for the project with `ghc-tags`
	@ghc-tags -c

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
