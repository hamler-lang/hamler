package = hamler
exe_target = hamler
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build

build:
	$(stack) build

run:
	$(stack) build --fast && $(stack) exec -- $(exe_target)

install:
	$(stack) install

clib:
	$(stack) run build -- -l

test:
	$(stack) test --fast $(package)

.PHONY : build install test
