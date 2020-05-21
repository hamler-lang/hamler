package = hamler
exe_target = hamler
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build

build:
	$(stack) run build -- -l

run:
	$(stack) build --fast && $(stack) exec -- $(exe_target)

install:
	$(stack) install

test:
	$(stack) test --fast $(package)

repl:
	$(stack) run repldev


.PHONY : build run install test repl
