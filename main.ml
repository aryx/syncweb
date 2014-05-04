open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* dup: readme.txt
 * syncweb is a command line tool enabling programmers to use the
 * literate programming[1] development methodology, using the noweb[2]
 * tool, while still being able to modify the generated files
 * from the literate document.
 * 
 * [1] http://en.wikipedia.org/wiki/Literate_programming
 * [2] http://www.cs.tufts.edu/~nr/noweb/
 * 
 * todo: could autodetect lang based on view filename
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* action mode *)
let action = ref ""

let lang = ref "ocaml"
let md5sum_in_auxfile = ref false
let less_marks = ref false

(*****************************************************************************)
(* Debugging helpers *)
(*****************************************************************************)

let with_error file f = 
  try
    pr2 (spf "processing %s" file);
    f ()
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

let actions () = [
  "-parse_orig", "   <file>", 
    Common.mk_action_1_arg (fun x -> 
      let tmpfile = "/tmp/xxx" in
      let orig = Engine.parse_orig x in
      Engine.unparse_orig orig tmpfile;
      Common.command2(spf "diff %s %s" x tmpfile);
    );
  "-parse_view", "   <file>", 
    Common.mk_action_1_arg (fun x -> 
      ignore(Engine.parse_view ~lang:Lang.mark_ocaml_short x);
    );

  "-view_of_orig", "   <file> <key>", 
    Common.mk_action_2_arg (fun x key -> 
      let orig = Engine.parse_orig x in
      let view = Engine.view_of_orig key orig in
      let tmpfile = "/tmp/xxx" in
      Engine.unparse_view ~lang:Lang.mark_ocaml view tmpfile;
      tmpfile +> Common.cat +> List.iter pr;
      (*Common.command2(spf "diff %s %s" x tmpfile); *)
    );

  (* superseded by Main.main_action now *)
  "-sync", "   <orig> <view>", 
    Common.mk_action_2_arg (fun origf viewf -> 
      let orig = Engine.parse_orig origf in
      let views = Engine.parse_view ~lang:Lang.mark_ocaml viewf in

      let orig' = Engine.sync ~lang:Lang.mark_ocaml     orig views  in

      let tmpfile = "/tmp/xxx" in
      Engine.unparse_orig orig' tmpfile;
      Common.command2(spf "diff %s %s" origf tmpfile);
    );
  "-unmark", "   <file>", 
    Common.mk_action_1_arg (fun file -> 

      let xs = Common.cat file in
      let xs = xs +> Common.exclude (fun s ->
        s =~ "^[ \t]*(\\*[sex]:"
      )
      in
      let tmpfile = "/tmp/xxx" in
      let s = Common2.unlines xs in
      Common.write_file tmpfile s;
      Common.command2(spf "diff -u %s %s" file tmpfile);
      if Common2.y_or_no "apply modif?"
      then Common.write_file file s
      else failwith "ok, skipping"
    );
]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  let lang = List.assoc !lang (Lang.lang_table) in
  let md5sum_in_auxfile = !md5sum_in_auxfile in
  let less_marks = !less_marks in

  match xs with
  (* simple case, one tex.nw file, one view *)
  | [origf;viewf] -> 

      let orig = Engine.parse_orig origf in
      (* we take the basename so that files can be put in any directory *)
      let topkey = Filename.basename viewf in
      if not (Sys.file_exists viewf)
      then
        let view = Engine.view_of_orig ~topkey orig in
        Engine.unparse_view ~md5sum_in_auxfile ~less_marks ~lang view viewf
      else begin
        (* old: let date1 = Common.filemtime origf in
         *      let date2 = Common.filemtime viewf in
         *)
        (* pr2 (spf "syncing %s and %s" origf viewf); *)
        let view = Engine.parse_view ~lang viewf in 
        let orig' = Engine.sync ~lang   orig view in
        let view' = Engine.view_of_orig ~topkey orig' in
        (* regenerate orig and view *)
        if orig <> orig' then begin
          pr2 "orig has been updated";
          Engine.unparse_orig orig' origf;
        end;
        if view <> view' then begin
          pr2 "view has been regenerated";
          Engine.unparse_view ~md5sum_in_auxfile ~less_marks ~lang view' viewf;
        end;
      end

  (* many .tex.nw, one view (to be called repeatedely for each view) *)
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
      let topkey = Filename.basename viewf in

      if not (Sys.file_exists viewf)
      then 
        let view = Engine.view_of_orig ~topkey orig in
        Engine.unparse_view ~md5sum_in_auxfile ~less_marks ~lang view viewf
      else begin
        (* pr2 (spf "syncing %s" viewf); *)

        with_error viewf (fun () ->
        let view = Engine.parse_view ~lang viewf in 
        let orig' = Engine.sync ~lang  orig view in
        let view' = Engine.view_of_orig  ~topkey orig' in
        (* regenerate orig and view *)
        if view <> view' then begin
          pr2 "view has been regenerated";
          Engine.unparse_view ~md5sum_in_auxfile ~less_marks ~lang view' viewf;
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
  actions() @
  []

let options () = 
  [
    "-lang", Arg.Set_string lang, 
    (spf " <lang> (default=%s, choices=%s)" !lang 
        (Common.join "|" (List.map fst Lang.lang_table)));

    "-md5sum_in_auxfile", Arg.Set md5sum_in_auxfile, 
    " ";
    "-less_marks", Arg.Set less_marks, 
    " ";


    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "syncweb version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";
  ] @
  Common2.cmdline_flags_devel () @
  Common.options_of_actions action (all_actions()) @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

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
