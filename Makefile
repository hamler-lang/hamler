.PHONY: build run repl shell shell-pure external-shell

build:
	nix-build release.nix

run: build
	result/bin/hamler

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl lib:hamler"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

cab2nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
