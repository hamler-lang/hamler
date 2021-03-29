package = hamler
exe_target = hamler

ifeq ($(shell uname -s),Darwin)
export HAMLER_HOME ?= /usr/local/lib/hamler
else
export HAMLER_HOME ?= /usr/lib/hamler
endif

all: build foreign

build:
	cabal run hamler build -- -l

foreign:
	@erlc -o ebin lib/Foreign/*.erl

clean:
	cabal clean

run:
	cabal build  && cabal run $(exe_target)

install:
ifeq ($(shell uname -s),Linux)
	cabal install --installdir=/usr/bin --overwrite-policy=always
endif
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

pkg:build test install
	make -C deploy/packages

.PHONY : build clean run install test repl  docker pkg
