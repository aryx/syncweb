open Common

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for lpizer *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let run_main (caps : <CLI.caps; ..>) (cmd : string) : (Exit.t, string) result =
  let args = String.split_on_char ' ' cmd in
  (* we run the CLI in a child process because it modifies globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   *)
  Proc.apply_in_child_process caps (fun () ->
      print_string (spf "executing: lpizer %s\n" cmd);
      try 
        Ok (Exit.catch (fun () -> 
              CLI.main caps (Array.of_list ("lpizer" :: args))))
      with 
      (* actually impossible *)
      | Failure s -> failwith (spf "impossible, failure %s should be catched" s)
      | End_of_file -> Error "End_of_file"          
   )
   ()

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let e2e_tests caps = 
  Testo.categorize "e2e" [
    t ~checked_output:(Testo.stdxxx ()) "--help" (fun () ->
        match run_main caps "--help" with
        | Ok Exit.OK -> ()
        | Ok x -> failwith (spf "unexpected exit: %s" (Exit.show x))
        | Error s -> failwith (spf "unexpected failure: %s" s)
    )
  ]

(*****************************************************************************)
(* The suite *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "mk" [
      e2e_tests caps;
  ]
