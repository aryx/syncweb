all:
	dune build

install:
	dune install

clean:
	dune clean

setup:
	opam install --deps-only .
