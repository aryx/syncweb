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
