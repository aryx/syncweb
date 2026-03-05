(lang dune 2.7)
; use 2.7 instead of 3.7 so dune does not generate confusing .pp.ml
; for codegraph_build (and syncweb_indexer)

(name syncweb)
(using menhir 2.0)

(generate_opam_files)

(source (github aryx/syncweb))
(homepage "https://github.com/aryx/syncweb")
(maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
(authors "Yoann Padioleau <yoann.padioleau@gmail.com>")

; syncweb itself: lightweight, only needs semgrep-pfff-libs
(package (name syncweb)
  (synopsis "Syncweb, Literate Programming meets Unison")
  (description "\
Syncweb is a command-line tool enabling programmers to use the
literate programming development methodology, using the noweb
tool, while still being able to modify the generated files
from the literate document.
")
  (license "GPL-2.0-only")
  (depends
    (ocaml (>= "4.12.0"))
    (dune (>= "3.7"))
    ; syncweb's own deps
    logs
    fpath
    ppx_deriving
    ; External deps of vendored submodule (semgrep-pfff-libs).
    ; coupling: semgrep-pfff-libs/dune-project
    ANSITerminal
    alcotest
    alcotest-lwt
    atdgen
    bos
    (cmdliner (and (>= "1.3.0") (< "2.0.0")))
    digestif
    fmt
    git
    lwt
    lwt_ppx
    memtrace
    ocolor
    pcre
    pcre2
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    ppx_sexp_conv
    ppxlib
    re
    semver
    sexplib
    testo
    timedesc
    trace
    (opentelemetry (< "0.11"))
    (opentelemetry-client-ocurl (< "0.11"))
    ambient-context-lwt
    uri
    uuidm
    yojson
  )
)

; lpizer: heavier, also needs semgrep-pfff-langs (parsers, tree-sitter)
(package (name lpizer)
  (synopsis "Literate programming code annotator")
  (description "\
Lpizer annotates source code with literate programming chunk markers,
using semgrep parsers for language-aware analysis.
")
  (license "GPL-2.0-only")
  (depends
    (ocaml (>= "4.12.0"))
    (dune (>= "3.7"))
    logs
    fpath
    ppx_deriving
    ; External deps of vendored submodules (semgrep-pfff-libs, semgrep-pfff-langs).
    ; coupling: semgrep-pfff-libs/dune-project, semgrep-pfff-langs/dune-project
    ; -- from semgrep-pfff-libs --
    ANSITerminal
    alcotest
    alcotest-lwt
    atdgen
    bos
    (cmdliner (and (>= "1.3.0") (< "2.0.0")))
    digestif
    fmt
    git
    lwt
    lwt_ppx
    memtrace
    ocolor
    pcre
    pcre2
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    ppx_sexp_conv
    ppxlib
    re
    semver
    sexplib
    testo
    timedesc
    trace
    (opentelemetry (< "0.11"))
    (opentelemetry-client-ocurl (< "0.11"))
    ambient-context-lwt
    uri
    uuidm
    yojson
    ; -- from semgrep-pfff-langs --
    compiler-libs
    (menhir (= "20230608"))
    parmap
    tsort
    visitors
    yaml
  )
)
