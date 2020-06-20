package = hamler
exe_target = hamler
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

ifeq ($(shell uname -s),Darwin)
export HAMLER_HOME ?= /usr/local/lib/hamler
else
export HAMLER_HOME ?= /usr/lib/hamler
endif

all: build

build:
	$(stack) run build -- -l

run:
	$(stack) build --fast && $(stack) exec -- $(exe_target)

install:
ifeq ($(shell uname -s),Linux)
	$(stack) install --local-bin-path /usr/bin --allow-different-user
endif
	$(stack) install --local-bin-path $(HAMLER_HOME)/bin --allow-different-user
	@cp repl/replsrv $(HAMLER_HOME)/bin/replsrv
	@cp -r ebin  $(HAMLER_HOME)/ebin
	@cp -r lib  $(HAMLER_HOME)/lib

test:
	$(stack) test --fast $(package)

repl:
	$(stack) run repldev

docker:
	docker build -t hamlerlang/hamler:$$(git describe --tags --always) -f deploy/docker/Dockerfile .

pkg:build test install
	make -C deploy/packages

.PHONY : build run install test repl docker pkg
