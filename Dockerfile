# Build syncweb (and semgrep/codemap/codegraph) with OCaml 4.14.2 via OPAM on Ubuntu.

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.2 -v


# Install semgrep libs (and its many dependencies) for lpizer and indexer
WORKDIR /semgrep
RUN git clone --depth=1 --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# and install-deps-UBUNTU-for-semgrep-core Makefile target
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make && make dune-build-all
RUN eval $(opam env) && dune install
#TODO: can't because then can't find -ltree-sitter
# RUN rm -rf /semgrep


# Install codemap libs (commons2_) for indexer
WORKDIR /codemap
RUN apt-get install -y libcairo2-dev libgtk2.0-dev
# alt: add codemap as a submodule in codegraph source
RUN git clone https://github.com/aryx/codemap /codemap
RUN ./configure
RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && dune install
RUN rm -rf /codemap

# Back to syncweb
WORKDIR /src

# Install other dependencies
COPY syncweb.opam configure ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && make install

# Test
RUN eval $(opam env) && syncweb --help && lpizer --help && indexer --help
# TODO run more tests, make test!
