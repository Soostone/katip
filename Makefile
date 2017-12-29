stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build

test:
	$(stack) test $(package)

.PHONY : build test

