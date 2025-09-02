
(* TODO: switch to Cap.main and Exit status like in xix *)
let _ =
   (* UCommon.main_boilerplate (fun () ->  *)
   Cap.main (fun _all_caps -> 
      CLI_lpizer.main Sys.argv;
  )
