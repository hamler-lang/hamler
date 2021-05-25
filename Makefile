package = hamler
exe_target = hamler

export HAMLER_HOME ?= $(shell $(CURDIR)/get-hamler-home.sh)

all: build foreign

build:
	cabal run hamler build -- -l -e

foreign:
	@erlc -o ebin lib/Foreign/*.erl

clean:
	cabal clean

run:
	cabal build  && cabal run $(exe_target)

install:
	@mkdir -p $(HAMLER_HOME)
	cabal install --installdir=$(HAMLER_HOME)/bin --overwrite-policy=always
	@cp repl/replsrv $(HAMLER_HOME)/bin/replsrv
	@cp -r ebin  $(HAMLER_HOME)
	@cp -r lib  $(HAMLER_HOME)
	@echo "export PATH=$(HAMLER_HOME)/bin:$$PATH" >> ~/.profile

test:
	cabal run hamler testDev

repl:
	cabal run hamler repldev

docker:
	docker build -t hamlerlang/hamler:$$(git describe --tags --always) -f deploy/docker/Dockerfile .

pkg:build test install
	make -C deploy/packages

.PHONY : build clean run install test repl  docker pkg
