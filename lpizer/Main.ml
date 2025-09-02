
(* TODO: switch to Cap.main and Exit status like in xix *)
let _ =
  UCommon.main_boilerplate (fun () -> 
    CLI_lpizer.main Sys.argv;
  )
