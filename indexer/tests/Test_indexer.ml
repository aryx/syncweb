open Common

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for syncweb indexer *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let run_main (caps : <Cap.fork; ..>) (cmd : string) : (unit (* Exit.t *), string) result =
  let args = String.split_on_char ' ' cmd in
  (* we run the CLI below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   *)
  CapProcess.apply_in_child_process caps (fun () ->
      print_string (spf "executing: mk %s\n" cmd);
      try 
        Ok ((* Exit.catch (fun () ->  *)
              CLI_indexer.main (Array.of_list ("indexer" :: args)))
      with 
      | Common.UnixExit 0 -> Ok ()
      | Common.UnixExit n -> Error (spf "UnixExit %d" n)
      (* actually impossible *)
      | Failure s -> failwith (spf "impossible, failure %s should be catched" s)
      | End_of_file -> Error "End_of_file"          
   )
   ()

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let e2e_tests (caps : < Cap.fork; ..>) = 
  Testo.categorize "e2e" [
    t ~checked_output:(Testo.stdxxx ()) "--help" (fun () ->
        match run_main caps "--help" with
(*
        | Ok Exit.OK -> ()
        | Ok x -> failwith (spf "unexpected exit: %s" (Exit.show x))
*)
        | Ok () -> ()
        | Error s -> failwith (spf "unexpected failure: %s" s)
    )
  ]

(*****************************************************************************)
(* The suite *)
(*****************************************************************************)

let tests (caps : < Cap.fork; ..>) : Testo.t list =
  Testo.categorize_suites "indexer" [
      e2e_tests caps;
  ]
