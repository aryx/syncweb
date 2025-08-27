# Build syncweb (and semgrep/codemap/codegraph) with OCaml 4.14.2 via OPAM on Ubuntu.
# Similar to efuns Dockerfile

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
RUN opam init --disable-sandboxing -y # (disable sandboxing due to Docker)
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v


# Install semgrep libs (and its many dependencies) for lpizer and indexer
WORKDIR /semgrep
RUN git clone --depth=1 --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# and install-deps-UBUNTU-for-semgrep-core Makefile target
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make && make dune-build-all
RUN eval $(opam env) && dune install \
   TCB commons commons2 profiling tracing process_limits parallelism pcre2 \
   testo testo-util testo-diff \
   gitignore paths glob git_wrapper \
   lib_parsing ast_generic tree-sitter lib_parsing_tree_sitter \
   tree-sitter-lang \
   parser_ocaml parser_scala parser_lisp \
   parser_cpp parser_java \
   parser_python parser_javascript parser_ruby parser_php \
   parser_go parser_rust \
   parser_yaml parser_jsonnet \
   parser_dockerfile parser_bash \
   pfff-lang_GENERIC-naming \
   semgrep aliengrep spacegrep
#TODO: remove semgrep aliengrep spacegrep
#TODO: can't because then can't find -ltree-sitter
# RUN rm -rf /semgrep

# Install codemap libs for codegraph and then for indexer and lpizer
WORKDIR /codemap
RUN apt-get install -y libcairo2-dev libgtk2.0-dev
RUN git clone --depth=1 https://github.com/aryx/codemap /codemap
RUN ./configure
RUN eval $(opam env) && make && make all
RUN eval $(opam env) && dune install \
    commons2_ files-format \
    visualization \
    graph_code highlight_code database_code layer_code \
    parser_c
RUN rm -rf /codemap

# Install codegraph libs (finder for indexer, lang_ml-analyze for lpizer)
WORKDIR /codegraph
RUN apt-get install -y libcairo2-dev libgtk2.0-dev
RUN git clone --depth=1 https://github.com/aryx/codegraph /codegraph
RUN ./configure
RUN eval $(opam env) && make && make all
RUN eval $(opam env) && dune install
RUN rm -rf /codegraph


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
RUN eval $(opam env) && syncweb --help && lpizer --help && syncweb_indexer --help
# TODO run more tests, make test!
