(*s: distribution_test.ml *)
open Common

let rec fib n = 
  if n = 0 then 0
  else 
   if n = 1 then 1
    else fib (n-1) + fib (n-2)


let map_ex arg = 
  pr (spf "map: %d" arg);
  fib arg

let reduce_ex acc e = 
  pr (spf "reduce: acc=%d, e=%d" acc e);
  acc + e

(*x: distribution_test.ml *)
let naive_map_reduce ~fmap ~freduce acc xs = 
  List.fold_left freduce acc (List.map fmap xs)
(*x: distribution_test.ml *)
let test_no_mpi () = 
  let res = naive_map_reduce ~fmap:map_ex ~freduce:reduce_ex 
    0 [1;2;3;10] in
  pr (spf "result = %d" res);
  ()
(*x: distribution_test.ml *)
let test_no_mpi_bis () = 
  let res = naive_map_reduce ~fmap:fib ~freduce:(fun acc e -> acc + e)
    0 [1;2;3;10] in
  pr (spf "result = %d" res);
  ()
(*x: distribution_test.ml *)
let test_mpi () = 
  let res = Distribution.map_reduce ~fmap:map_ex ~freduce:reduce_ex 
    0 [35;35;35;35] in
  pr (spf "result = %d" res);
  ()

let main = 
  (*s: set debug mpi flag if necessary *)
  Distribution.debug_mpi := true;
  (*e: set debug mpi flag if necessary *)
  test_mpi ()

(*x: distribution_test.ml *)
let test_mpi_raw () =
  let rank = Mpi.comm_rank Mpi.comm_world in
  pr (spf "rank: %d" rank);
  if rank = 0
  then 
    Distribution.master reduce_ex [1;2;3;10] 
  else begin
    Distribution.worker map_ex;
    raise Distribution.TaskFinished
  end

(*e: distribution_test.ml *)
