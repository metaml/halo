.DEFAULT_GOAL = help

export SHELL := /bin/bash

OPT =
BIN ?= halo

dev: clean ## build continuously
	@cabal v2-build 2>&1 | source-highlight --src-lang=haskell --out-format=esc
	@fswatcher --path . --include "\.hs$$|\.cabal$$" --throttle 31 cabal -- $(OPT) v2-build 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

dev-ghcid: clean ## build continuously using ghcid
	@ghcid --command="cabal $(OPTS) v2-repl -fwarn-unused-binds -fwarn-unused-imports -fwarn-orphans" \
	       --reload=app/halo.hs \
	       --restart=halo.cabal \
	| source-highlight --src-lang=haskell --out-format=esc

build: clean # lint (breaks on multiple readers) ## build
	cabal $(OPT) v2-build --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

test: ## test
	cabal $(OPT) v2-test

lint: ## lint
	hlint app src

clean: ## clean
	cabal $(OPT) v2-clean

run: ## run main, default: BIN=lamh
	cabal $(OPT) v2-run ${BIN}

repl: ## repl
	cabal $(OPT) v2-repl

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	-@ghc --version
	-@cabal --version
	-@hlint --version
	-@ghcid --version --ignore-loaded
	-@echo BIN=$(BIN)

# @todo: not indempotent--fix later
init: ## init project but run "make install-nix" first
	${MAKE} -f etc/init.mk init

install-nix: # install nix
	${MAKE} -f etc/init.mk install-nix

shell: ## initialize project
	${MAKE} -f etc/init.mk nix-shell
