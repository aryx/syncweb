###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

# TODO: add {,test.bc} at some point
default:
	bash -c "dune build _build/install/default/bin/{syncweb,lpizer,syncweb_indexer}"

all:
	dune build
clean:
	dune clean
install:
	dune install

#TODO: use testo, run syncweb on demos/ ? or in tests/syncweb/?
#TODO: ./bin/lpizer -verbose -lang c tests/lpizer/*.c
test:
	./bin/syncweb_indexer -lang cmt .
	./bin/lpizer -verbose -lang ocaml tests/lpizer/*.ml

setup:
	opam install --deps-only .

.PHONY: all clean install test

build-docker:
	docker build -t "syncweb" .

###############################################################################
# Developer targets
###############################################################################

# See https://github.com/aryx/codemap and https://github.com/aryx/fork-efuns
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .

index:
	$(MAKE) clean
	$(MAKE)
	codegraph_build -lang cmt .
