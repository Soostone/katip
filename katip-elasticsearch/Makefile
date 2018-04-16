package = katip-elasticsearch

stack_yaml = STACK_YAML="../stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

build-profile:
	$(stack) build --profile --ghc-options='-threaded -fprof-auto' --library-profiling --executable-profiling --benchmark-arguments "+RTS -h -pa -RTS" $(package)

run-bulk-example-profile:
	$(stack) exec -- bulk-example +RTS -hc -p -RTS

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

bench:
	$(stack) bench $(package)

bench-profile:
	$(stack) bench --profile --ghc-options='-threaded -fprof-auto' --library-profiling --executable-profiling --benchmark-arguments "+RTS -h -pa -RTS" $(package)

bench-ghci:
	$(stack) ghci $(package):bench:$(package)-bench

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):$(package)"

dev-deps:
	stack install ghcid

.PHONY : build build-dirty run install ghci test test-ghci bench ghcid dev-deps

