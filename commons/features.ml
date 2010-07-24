# 1 "commons/features.ml.in"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "commons/features.ml.in"
(* yes sometimes cpp is useful *)

(* old:
note: in addition to Makefile.config, globals/config.ml is also modified
by configure
features.ml: features.ml.cpp Makefile.config
 cpp -DFEATURE_GUI=$(FEATURE_GUI) -DFEATURE_MPI=$(FEATURE_MPI) -DFEATURE_PCRE=$(FEATURE_PCRE) features.ml.cpp > features.ml




clean::
 rm -f features.ml

beforedepend:: features.ml

*)
# 34 "commons/features.ml.in"
module Distribution = struct
  let under_mpirun () =
    false

  let mpi_main2 ?debug_mpi map_ex reduce_ex fxs =
    let res = List.map map_ex (fxs()) in
    reduce_ex res

  let mpi_adjust_argv argv =
    argv
end
# 60 "commons/features.ml.in"
module Backtrace = struct
 let print () =
   print_string "no backtrace support, use configure --with-backtrace\n"
end
