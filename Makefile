all: build

build:
	@dune build @install @runtest

clean:
	@dune clean

install: build
	@dune install
