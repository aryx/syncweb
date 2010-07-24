open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* A poor's man map-reduce in ocaml.
 *  
 *
 * alternatives:
 * - PVM
 * - charm 
 * - netplex
 * - chameleon
 * - jocaml
 * - ocaml3pl
 * - ensemble
 *)

(*****************************************************************************)
(* MPI *)
(*****************************************************************************)

(*
 * procedure to use:
 * - add ocamlmpi/ to your software.
 * - adapt Makefile to compile ocamlmpi and this file:
 *    LIBS ocamlmpi/mpi.cma \
 *         commons/commons_mpi.cma \
 * 
 *    MAKESUBDIRS ocamlmpi
 *    INCLUDEDIRS ocamlmpi
 *    rec 
 *     $(MAKE) -C commons 
 *     $(MAKE) all -C ocamlmpi
 *     $(MAKE) distribution -C commons
 *    rec.opt
 *     $(MAKE) all.opt -C commons 
 *     $(MAKE) all.opt -C ocamlmpi
 *     $(MAKE) distribution.opt -C commons
 *    cp_4mpi:
 *       echo no copy needed yet
 *    test_mpi: opt yacfe.opt cp_4mpi 
 *       time mpirun -p4pg env-machines-onlyme.pg ./yacfe.opt -parse_all /home/pad/software-src/devel/sparse-git
 * - add mpi argument processing in main.ml 
 *     let argv = Distribution.mpi_adjust_argv Sys.argv in
 * - can add   Distribution.mpi_actions() ++
 * - split code so that can have a mapper and reduce.
 *   usually have 2 functions, one that do_on_one_file (file, i, nbfiles) = 
 *   and another one that build the list, and have a map_ex, and reduce_ex.
 *   and finally the call to mpi_main:
 *     (* will call either map_ex for worker or reduce_ex for server *)
 *      biglist +> Distribution.mpi_main2 ~debug_mpi:true map_ex reduce_ex
 * 
 * 
 *)

(* 
 * Who run the process on the different machines ? Where is the list
 * of machines ? MPI default API does not handle that and does not
 * want to handle that, which I think is good because with no machine names
 * in the code, no need to recompile if the list changes. Instead everything
 * is done via the process launcher command line MPI tool: mpirun
 * 
 * run via for instance
 * 
 *  ./mpirun -np 3 ./prog arg1 arg2 
 * 
 * or if have multiple machines, create a file.pg containing the info 
 * and run
 * 
 *  ./mpirun -p4pg file.pg ./prog arg1 arg2
 * 
 * file.pg can be for instance
 *    #aryx: 2 processors
 *    aryx 0 /home/yyzhou/pad/c-acomment/cql.opt
 *    aryx 1 /home/yyzhou/pad/c-acomment/cql.opt
 *    aryx 1 /home/yyzhou/pad/c-acomment/cql.opt
 *    
 *    #carmen: 4 processors 
 *    carmen.cs.uiuc.edu 1  /home/yyzhou/pad/c-acomment/cql.opt
 *    carmen.cs.uiuc.edu 1  /home/yyzhou/pad/c-acomment/cql.opt
 *    carmen.cs.uiuc.edu 1  /home/yyzhou/pad/c-acomment/cql.opt
 *    carmen.cs.uiuc.edu 1  /home/yyzhou/pad/c-acomment/cql.opt
 * 
 * Do we need threads in the server to listen asynchonously to the 
 * first-ready worker ?
 * Could, but MPI provides some helpers for doing that without threads
 * (I guess using some select internally).
 *  
 * Do we need a trick to redirect worker output to server output ? 
 * It's handled already :) mpirun is a quite good program. And with
 * my Common._prefix_pr adjustment, very easy to see who is outputting
 * what.
 * 
 * Do we need to manage errors such as killing the clients on the remove
 * machines when there is for instance a pb, a bug in the server ? 
 * Again, mpirun seems to automatically handle this.
 * 
 * 
 * 
 * 
 * 
 * export RSHCOMMAND=ssh ? or configure -rsh=ssh mpich
 * Is it faster to use rsh ? I guess no need encryption of data so faster ?
 * 
 * note: mpifying a program seems to have good virtue: it force you to 
 * use less globals and to show more clearly the dependencies.
 * 
 * use the -stdout -stderr of mpirun ?
 *
 * Here is a generic MapReduce. Take a list of x, apply f, and reduce.
 * What if more workers than files ? Currently assert not the case.
 * How kill workers ? Currently send the special None to indicate such
 * special code.
 * 
 * Code is very polymorphic, but not type-safe as ocamlmpi use marshalling,
 * so take care!!
 * 
 * 
 * todo: handle dead worker ? and redirect his file to another worker
 * 
 * todo: collect some statistics on workers ? maybe can first 
 * broadcast an identification request where worker just do a `hostname`
 * 
 * todo: typecheck ? add always as first element a string containing
 * the type so worker can assert((fst req) = "string").
 *)



exception TaskFinished



let worker ?(debug_mpi=false) map_ex = 
  let rank = Mpi.comm_rank Mpi.comm_world in
  if debug_mpi 
  then  Common._prefix_pr := (spf "W%d:" rank);

  pr2 (spf "mpi worker: %d" rank);
  Common.unwind_protect (fun () -> 
  while true do
    let req = Mpi.receive 0 0 Mpi.comm_world in
    match req with
    | Some req -> 
        (* big work *)
        let res = map_ex req in
        Mpi.send res 0 0 Mpi.comm_world
    | None -> 
        (* special code meaning end of transmission *)
        pr2 (spf "worker exiting: %d" rank);
        flush stderr;
        flush stdout;
        (*Mpi.barrier Mpi.comm_world; *)
        raise (UnixExit (0))
  done
  )
  (fun e -> 
    match e with 
    | UnixExit(0) -> ()
    | _ -> 
        pr2 (spf "PB: mpi worker %d dieing: %s" rank (Common.exn_to_s e));
  )




let killing_workers numworkers = 
  for i = 1 to numworkers do
    Mpi.send None i 0 Mpi.comm_world;
  done

(* todo: make xs lazy ? *)
let server ?(debug_server=false) xs reduce_ex = 
  let numworkers = Mpi.comm_size Mpi.comm_world - 1 in
  pr2 (spf "mpi server. num clients=%d" numworkers);

  if debug_server
  then Common._prefix_pr := ("S:");

  let remaining = ref xs in
  let working = ref 0 in
  let res_list = ref [] in
  Common.unwind_protect (fun () -> 
 
    assert(List.length !remaining > numworkers);
    
    (* Send initial work *)
    for i = 1 to numworkers do
      let arg = Common.pop2 remaining in
      Mpi.send (Some arg) i 0 Mpi.comm_world;
      incr working;
    done;

    (* Enter server loop *)
    while !working > 0 do
      let (res, src, _) = Mpi.receive_status Mpi.any_source 0 Mpi.comm_world in
      Common.push2 res res_list;
      
      if not (null !remaining) then begin
        let arg = Common.pop2 remaining in
        Mpi.send (Some arg) src 0 Mpi.comm_world;
      end 
      else decr working;
    done;
    
    
    killing_workers numworkers;
    flush stderr;
    flush stdout;
    (*Mpi.barrier Mpi.comm_world; *)

    (* big work *)
    reduce_ex !res_list

  ) (fun e -> 
    pr2 (spf "PB: mpi server dieing: %s" (Common.exn_to_s e));
    killing_workers numworkers; 
    (*Mpi.barrier Mpi.comm_world*)
  )



let under_mpirun () = 
  Sys.argv +> Array.to_list +> List.exists (fun x -> 
    x ="-p4pg"  || x = "-p4rmrank"
  )

(* todo: make xs lazy ? *)
let mpi_main ?debug_mpi map_ex reduce_ex xs =
  if under_mpirun ()
  then begin
    let rank = Mpi.comm_rank Mpi.comm_world in
    pr2 (spf "rank: %d" rank);
    if rank = 0 
    then
      server xs reduce_ex
    else 
      worker ?debug_mpi map_ex
  end
  else 
    let res = xs +> List.map map_ex in
    reduce_ex res



(* same but with xs lazy, so client don't do it *)
let (mpi_main2: 
  ?debug_mpi:bool -> ('a -> 'b) -> ('b list -> 'c) -> (unit -> 'a list) -> 'c)
  = fun ?debug_mpi map_ex reduce_ex fxs ->
  if under_mpirun ()
  then begin
    let rank = Mpi.comm_rank Mpi.comm_world in
    pr2 (spf "rank: %d" rank);
    if rank = 0 
    then
      server (fxs()) reduce_ex
    else 
      begin 
        worker ?debug_mpi map_ex; (* normally raise already a UnixExit *)
        raise TaskFinished
      end
  end
  else 
    let res = (fxs()) +> List.map map_ex in
    reduce_ex res
  




(* Currently mpirun does not send the same argument to the server and worker, 
 * and it also add extra arguments which can confuse my Common.parse_options.
 * So this function correct this deficiency by fixing this argv problem.
 * 
 * Not-perfect-but-basic-feels-right: not perfect but note that 
 * test_mpi() does not require to deal with this pb. 
 * You can still use test_mpi() without this if you
 * dont need any argument dispatch using Arg.
 * 
 *)
let mpi_adjust_argv argvold = 
  let rank = Mpi.comm_rank Mpi.comm_world in
  let numworkers = Mpi.comm_size Mpi.comm_world - 1 in

  if rank = 0
  then begin
    (* the master get the full list of arguments, but also some 
     * extra stuff that we must filter *)
    let xs = Array.to_list argvold in
    let xs = xs +> Common.take_until (fun s -> s = "-p4pg") in
    (* send good argv to workers *)
    for i = 1 to numworkers do
      Mpi.send xs i 0 Mpi.comm_world;
    done;
    Array.of_list xs
  end
  else begin
    (* recieve argv from server as mpirun does not pass it to us *)
    let (res, src, _) = Mpi.receive_status Mpi.any_source 0 Mpi.comm_world in
    Array.of_list res
  end
      

let mpi_debug_argv _argv =
  let rank = Mpi.comm_rank Mpi.comm_world in
  Sys.argv +> Array.to_list +> List.iter (fun s -> 
    pr2 (spf "%d: %s" rank s);
  );
  ()



(*---------------------------------------------------------------------- *)
(* Simple examples of use *)
(*---------------------------------------------------------------------- *)


let map_ex arg = 
  pr2 (spf "processing: %d" arg);
  arg + 1

let reduce_ex xs = 
  pr2 (spf "reducing: %d" (List.length xs));
  let res = Common.sum_int xs in
  pr2 (spf "result= %d" res);
  ()

(* can run via: mpirun -np 3 ./prog -test_mpi *)
let test_mpi _args =
  let rank = Mpi.comm_rank Mpi.comm_world in
  pr2 (spf "rank: %d" rank);
  if rank = 0
  then 
    server [1;2;3;4;5;6] reduce_ex
  else 
    worker map_ex




let map_ex2 arg = 
  pr2 (spf "processing: %d" arg);
  arg + 3

let reduce_ex2 xs = 
  pr2 (spf "reducing: %d" (List.length xs));
  let res = Common.sum_int xs in
  pr2 (spf "result= %d" res);
  ()

(* can run via: mpirun -np 3 ./prog -test_mpi2 *)
let test_mpi2 _args =
  let rank = Mpi.comm_rank Mpi.comm_world in
  pr2 (spf "rank: %d" rank);
  if rank = 0 
  then
    server [1;2;3;4;5;7] reduce_ex2
  else 
    worker map_ex2

(* even shorter form that can also work without mpi by defaulting to 
 * a traditional List.map *)
let test_mpi3 _args = 
  mpi_main map_ex2 reduce_ex2 [1;2;3;5;27]


let mpi_actions () = [
  "-test_mpi", "   ", 
  Common.mk_action_n_arg test_mpi;
  "-test_mpi2", "   ", 
  Common.mk_action_n_arg test_mpi2;
  "-test_mpi3", "   ", 
  Common.mk_action_n_arg test_mpi3;
]

