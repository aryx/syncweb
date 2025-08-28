(* Copyright 2009-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Assist in producing a first version of a literate programming (LP)
 * document.
 *
 * history:
 *  - was in pfff -lpize before.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* possible values: "c" *)
let lang_str = ref "ml"

let log_level = ref (Some Logs.Warning)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(* for lpification, to get a list of files and handling the skip list *)
(*
let find_source xs =
  let root = Common2_.common_prefix_of_files_or_dirs xs in
  let root = Unix.realpath root |> Common2_.chop_dirsymbol in
  let files = 
    failwith "TODO: find_source use Find_generic in codegraph"
  in
    (* Find_source.files_of_dir_or_files ~lang:!lang xs in *)
  files |> List.iter (fun file ->
    Logs.info (fun m -> m "processing: %s" (Filename_.readable root file))
  )
*)

let main_action (xs : Fpath.t list) : unit =
  (* TODO: pass down Lang.of_string !lang_str *)
  Lpize.lpize xs

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*
  "-find_source", " <dirs>",
  Common.mk_action_n_arg find_source;
*)
let all_actions () = 
  actions() @
  []

let options () = [
  "-lang", Arg.Set_string lang_str, 
  (spf " <str> choose language (default = %s) " !lang_str);
  ] @
  Common2.cmdline_flags_devel () @
  Arg_.options_of_actions action (all_actions()) @
  [
  "-verbose", Arg.Unit (fun () -> log_level := Some Logs.Info),
  " ";
  "-debug", Arg.Unit (fun () -> log_level := Some Logs.Debug),
  " ";
  "-quiet", Arg.Unit (fun () -> log_level := None),
  " ";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  let usage_msg = 
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
      " [options] -lang <lang> <files> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Arg_.parse_options (options()) usage_msg Sys.argv in

  Logs_.setup ~level:!log_level ();
  Logs.info (fun m -> m "Starting logging");

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () -> 
    
    (match args with
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Arg_.action_list (all_actions())) -> 
        Arg_.do_action !action xs (all_actions())

    | _ when not (String_.empty !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (Fpath_.of_strings (x::xs))

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Arg_.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  UCommon.main_boilerplate (fun () -> 
    main ();
  )
