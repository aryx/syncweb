
###############################################################################
# Main targets
###############################################################################

all:
	dune build
install:
	dune install
clean:
	dune clean

setup:
	opam install --deps-only .

###############################################################################
# Developer targets
###############################################################################

# See https://github.com/aryx/codemap and https://github.com/aryx/fork-efuns
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .
