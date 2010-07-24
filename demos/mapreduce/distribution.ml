(*s: distribution.ml *)
(*s: copyright header *)
(* Yoann Padioleau 
 * 
 * Copyright (C) 2009 University of Urbana Champaign
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
(*e: copyright header *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* cf the distribution.ml.nw literate document for the documentation *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(*s: debug global *)
let debug_mpi = ref false
(*e: debug global *)

(*****************************************************************************)
(* Protocol *)
(*****************************************************************************)
(*s: protocol for master/workers *)
let rank_master = 0
(*x: protocol for master/workers *)
exception TaskFinished
(*x: protocol for master/workers *)
type ('a, 'b) protocol = DataIn of 'a | DataRes of 'b | StopWorker
(*x: protocol for master/workers *)
exception ProtocolError
(*x: protocol for master/workers *)
let notag = 0
(*e: protocol for master/workers *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: worker *)
let worker ~fmap:map_ex = 
  let rank = Mpi.comm_rank Mpi.comm_world in
  let hostname = Unix.gethostname () in

  (*s: debug worker *)
  if !debug_mpi 
  then Common._prefix_pr := (spf "W%d:" rank);
  if !debug_mpi
  then pr2 (spf "DEBUG: mpi worker %s, rank=%d" hostname rank);
  (*e: debug worker *)

  Common.unwind_protect (fun () -> 
   (*s: enter worker loop *)
   while true do
     let req = Mpi.receive rank_master notag Mpi.comm_world in
     match req with
     | DataIn req -> 
         (* big work *)
         let res = map_ex req in
         Mpi.send (DataRes res) rank_master notag Mpi.comm_world
     | StopWorker -> 
         (*s: debug worker exit *)
         if !debug_mpi 
         then pr2 ("DEBUG: worker exiting");
         flush stderr; flush stdout;
         (*e: debug worker exit *)
         raise (UnixExit (0))
     | DataRes _ -> raise ProtocolError
   done
   (*e: enter worker loop *)
  )
  (fun e ->
    (*s: exit worker *)
    match e with 
    | UnixExit(0) -> exit 0
    | _ -> 
        pr2 (spf "PB: mpi worker dying: %s" (Common.exn_to_s e));
    (*e: exit worker *)
  )

(*e: worker *)

(*s: master *)
let master ~freduce:reduce_ex acc xs = 
  let available_workers = Mpi.comm_size Mpi.comm_world - 1 in
  let actual_workers = min (List.length xs) available_workers in

  (*s: debug master *)
  if !debug_mpi
  then Common._prefix_pr := ("MS:");
  if !debug_mpi
  then pr2 (spf "DEBUG: mpi master, number of clients=%d" available_workers);
  (*e: debug master *)
  (*s: killing_workers helper *)
  let killing_workers xs = 
    xs +> List.iter (fun i -> Mpi.send StopWorker i notag Mpi.comm_world)
  (*e: killing_workers helper *)
  in


  let in_list  = ref xs in
  let out_list = ref [] in
  let working = ref 0 in
  Common.unwind_protect (fun () -> 
 
    assert(List.length !in_list >= actual_workers);
    
    (*s: send initial work to valid workers *)
    for i = 1 to actual_workers do
      let arg = Common.pop2 in_list in
      Mpi.send (DataIn arg) i notag Mpi.comm_world;
      incr working;
    done;
    (*e: send initial work to valid workers *)
    (*s: kill idle workers *)
    killing_workers (Common.enum_safe (actual_workers+1) available_workers);
    (*e: kill idle workers *)
    (*s: enter server loop, [[in_list]] shrinks and [[out_list]] grows *)
    while !working > 0 do
      let (res, src, _) = Mpi.receive_status Mpi.any_source notag Mpi.comm_world in
      (match res with
      | DataRes x -> 
          Common.push2 x out_list;
      | DataIn _ | StopWorker -> raise ProtocolError
      );
      
      if not (null !in_list) then begin
        let arg = Common.pop2 in_list in
        Mpi.send (DataIn arg) src notag Mpi.comm_world;
      end 
      else decr working;
    done;
    (*e: enter server loop, [[in_list]] shrinks and [[out_list]] grows *)
    (*s: no more remaining, kill workers *)
    killing_workers (Common.enum 1 actual_workers);
    flush stderr;flush stdout;
    (*e: no more remaining, kill workers *)

    (* big work *)
    List.fold_left reduce_ex acc !out_list

  ) (fun e -> 
    (*s: kill workers because problem *)
    pr2 (spf "PB: mpi master dying: %s" (Common.exn_to_s e));
    killing_workers (Common.enum 1 available_workers); 
    (*e: kill workers because problem *)
  )
(*e: master *)

(*s: under_mpirun *)
let under_mpirun () = 
  Sys.argv +> Array.to_list +> List.exists (fun x -> 
    x ="-p4pg"  || x = "-p4rmrank"
  )
(*e: under_mpirun *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(*s: map_reduce *)
let map_reduce ~fmap:map_ex ~freduce:reduce_ex acc xs =
  if under_mpirun ()
  then begin
    (*s: map_reduce mpi case *)
    let rank = Mpi.comm_rank Mpi.comm_world in
    if rank = rank_master
    then
      master ~freduce:reduce_ex acc xs
    else begin
      worker ~fmap:map_ex;
      raise TaskFinished (* for the type system *)
    end
    (*e: map_reduce mpi case *)
  end
  else 
    List.fold_left reduce_ex acc (List.map map_ex xs)
(*e: map_reduce *)

(*s: map_reduce_lazy *)
(* same but with xs lazy, so workers don't need to compute it *)
let map_reduce_lazy ~fmap:map_ex ~freduce:reduce_ex acc fxs =
  if under_mpirun ()
  then begin
    let rank = Mpi.comm_rank Mpi.comm_world in
    if rank = rank_master
    then
      master ~freduce:reduce_ex acc (fxs()) (* changed code *)
    else 
      begin 
        worker ~fmap:map_ex; (* normally raise already a UnixExit *)
        raise TaskFinished
      end
  end
  else 
    let xs = fxs() in (* changed code *)
    List.fold_left reduce_ex acc (List.map map_ex xs)

(*e: map_reduce_lazy *)

(*****************************************************************************)
(* Extra *)
(*****************************************************************************)
(*s: protocol for argv *)
type protocol_argv = Argv of string list
(*e: protocol for argv *)
(*s: mpi_adjust_argv *)
let mpi_adjust_argv argvold = 
  let rank = Mpi.comm_rank Mpi.comm_world in
  let numworkers = Mpi.comm_size Mpi.comm_world - 1 in
  if rank = rank_master
  then 
    (*s: adjust argv for master *)
    begin
      (* the master get the full list of arguments, but also some 
       * extra stuff that we must filter *)
      let xs = Array.to_list argvold in
      let xs = xs +> Common.take_until (fun s -> s = "-p4pg") in
      (* send good argv to workers *)
      for i = 1 to numworkers do
        Mpi.send (Argv xs) i notag Mpi.comm_world;
      done;
      Array.of_list xs
    end
    (*e: adjust argv for master *)
  else
    (*s: adjust argv for worker *)
    begin
      (* recieve argv from master as mpirun does not pass it to us *)
      let (Argv res, src, _) = 
        Mpi.receive_status Mpi.any_source notag Mpi.comm_world in
      Array.of_list res
    end
    (*e: adjust argv for worker *)
(*e: mpi_adjust_argv *)

(*e: distribution.ml *)
