package = hamler
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build

build:
	$(stack) build

install:
	$(stack) install

test:
	$(stack) test --fast $(package)

.PHONY : build install test
