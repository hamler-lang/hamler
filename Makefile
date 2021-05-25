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

test:
	cabal run hamler testDev

repl:
	cabal run hamler repldev

docker:
	docker build -t hamlerlang/hamler:$$(git describe --tags --always) -f deploy/docker/Dockerfile .

pkg:
	mkdir -p /usr/lib/hamler/bin
	HAMLER_HOME="/usr/lib/hamler" cabal run hamler build -- -l -e
	cabal install --overwrite-policy=always
	cp ~/.cabal/bin/hamler /usr/lib/hamler/bin
	cp repl/replsrv /usr/lib/hamler/bin/replsrv
	cp -r ebin /usr/lib/hamler
	cp -r lib /usr/lib/hamler
	make -C deploy/packages

.PHONY : build clean run install test repl  docker pkg
