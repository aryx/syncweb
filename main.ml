open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

(* action mode *)
let action = ref ""

let lang = ref "ocaml"
let md5sum_in_auxfile = ref false
let less_marks = ref false

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

let with_error file f = 
  try f ()
  with e ->
    pr2 (spf "PB while was processing %s" file);
    raise e


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)

let console_actions () = [
]
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  let lang = List.assoc !lang (Engine.lang_table) in

  match xs with
  | [origf;viewf] -> 

      let orig = Engine.parse_orig origf in

      if not (Sys.file_exists viewf)
      then
        (* we take the basename so that files can be put in any directory *)
        let view = Engine.view_of_orig ~topkey:(Filename.basename viewf) orig in
        Engine.unparse_view 
          ~md5sum_in_auxfile:!md5sum_in_auxfile
          ~less_marks:!less_marks
          ~lang view viewf
      else begin
        (*
        let date1 = Common.filemtime origf in
        let date2 = Common.filemtime viewf in
        *)
        (* pr2 (spf "syncing %s and %s" origf viewf); *)
        let view = Engine.parse_view ~lang viewf in 
        let orig' = Engine.sync ~lang   orig view in
        let view' = Engine.view_of_orig 
          ~topkey:(Filename.basename viewf) 
          orig' 
        in
        (* regenerate orig and view *)
        if orig <> orig' then begin
          pr2 "orig has been updated";
          Engine.unparse_orig orig' origf;
        end;
        if view <> view' then begin
          pr2 "view has been regenerated";
          Engine.unparse_view 
            ~md5sum_in_auxfile:!md5sum_in_auxfile
            ~less_marks:!less_marks
            ~lang view' viewf;
        end;
      end

  | xs when List.length xs > 2 -> 
      let origfs, viewf = 
        match List.rev xs with
        | x::xs -> List.rev xs, x
        | _ -> raise Impossible
      in
      let origs = origfs +> List.map (fun f -> 
        with_error f (fun () -> f, Engine.parse_orig f)
      ) in
      let orig = Engine.pack_multi_orig origs in

      if not (Sys.file_exists viewf)
      then 
        let view = Engine.view_of_orig ~topkey:(Filename.basename viewf) orig in
        Engine.unparse_view 
          ~md5sum_in_auxfile:!md5sum_in_auxfile
          ~less_marks:!less_marks
          ~lang view viewf
      else begin
        (* pr2 (spf "syncing %s" viewf); *)

        with_error viewf (fun () ->
        let view = Engine.parse_view ~lang viewf in 
        let orig' = Engine.sync ~lang  orig view in
        let view' = Engine.view_of_orig 
          ~topkey:(Filename.basename viewf) 
          orig' 
        in
        (* regenerate orig and view *)
        if view <> view' then begin
          pr2 "view has been regenerated";
          Engine.unparse_view 
            ~md5sum_in_auxfile:!md5sum_in_auxfile
            ~less_marks:!less_marks
            ~lang view' viewf;
        end;
        let origs' = Engine.unpack_multi_orig orig' in
        Common2.zip origs origs' +> List.iter (fun ((f1, orig), (f2, orig')) ->
          if orig <> orig' then begin
            pr2 (spf "orig %s has been updated" f1);
            Engine.unparse_orig orig' f1;
          end;
        );
        )
      end

  | _ -> failwith "need the name of the orig file and name of view file"

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  console_actions() @
  (*Test.actions() @*)
  Engine.actions() @
  []

let options () = 
  [
    "-lang", Arg.Set_string lang, 
    (spf " <lang> (default=%s, choices=%s)" !lang 
        (Common.join "|" (List.map fst Engine.lang_table)));

    "-md5sum_in_auxfile", Arg.Set md5sum_in_auxfile, 
    " ";
    "-less_marks", Arg.Set less_marks, 
    " ";


    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "syncweb version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ] @
  Common2.cmdline_flags_devel () @
  Common.options_of_actions action (all_actions()) @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  (* Common_extra.set_link(); 
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg = 
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
      " [options] <orig> <view> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 
    
    (match args with
    
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)
    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
