(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* syncweb is a command line tool enabling programmers to use the
 * literate programming[1] development methodology, using the noweb[2]
 * tool, while still being able to modify the generated files
 * from the literate document.
 * dup: readme.txt
 * 
 * [1] http://en.wikipedia.org/wiki/Literate_programming
 * [2] http://www.cs.tufts.edu/~nr/noweb/
 * 
 * todo: 
 *  - bug: if have a chunkname that is used two times (so it factorizes
 *    code), and that you modify one instance, then it does not propagate
 *    to the other!! bug!! (see pb with char_code vs charcode in Efuns.nw)
 *  - add some Common.profile, parsing orig, parsing view, extract view, etc
 *  - optimize make sync when have many files, cache in a .marshall
 *    the parsing of the .nw? or use hashtbl instead of list for faster
 *    lookup
 *  - detect recursive chunks that leads to weird thing when do 'make sync'
 *  - could autodetect language based on view filenames?
 * 
 * related work:
 *  - todo: http://leoeditor.com/
 *  - org-babel and its detangle function
 *    http://comments.gmane.org/gmane.emacs.orgmode/32814 but syncweb
 *    workflow is better, it automatically syncs in the right direction.
 *  - http://www.t3x.org/s9fes/edoc.html?
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
    pr2 (spf "Problem found while was processing %s" file);
    raise e

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

open Web
(* Allows to have multiple filenames with the same name but different dir.
 * We used to take the basename so that files could be put in any directory.
 *)
let find_topkey_corresponding_to_file orig viewf =
  (* old: Filename.basename viewf *)
  let base = Filename.basename viewf in
  let defs = orig |> Common.map_filter (function
    | Tex _ -> None
    | ChunkDef (def, _xs) -> 
      let s = def.chunkdef_key in
      if Filename.basename s =*= base
      then Some s
      else None 
  )
  in
  (match defs with
  | [] -> failwith (spf "could not find topkey for %s" viewf)
  | [x] -> x
  | x::y::ys ->
    (* same basenames, need to use the directory as a discriminator *)
    let revdir = Filename.dirname viewf |> Common.split "/" |> List.rev in
    let candidates = 
      (x::y::ys) |> List.map (fun file ->
        Filename.dirname file |> Common.split "/" |> List.rev,
        file
       )
    in
    let err () = 
      failwith (spf "too many matching topkeys for %s (%s)" viewf
                  ((x::y::ys) |> Common.join ", "))
    in
    let rec aux revdir candidates =
      match revdir with
      | [] -> err ()
      | x::xs ->
        let same_top_revdir_candidates =
          candidates |> Common.map_filter (fun (revdir, fullfile) ->
            match revdir with
            | [] -> None
            | y::ys -> if y = x then Some (ys, fullfile) else None
          )
        in
        (match same_top_revdir_candidates with
        | [] -> err ()
        | [_, fullfile] -> fullfile
        | _ -> aux xs same_top_revdir_candidates
        )
    in
    aux revdir candidates
  )

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  (* testing *)
  "-parse_orig", "   <file>",
    Common.mk_action_1_arg (fun x -> 
      let tmpfile = "/tmp/xxx" in
      let orig = Web.parse x in
      Web.unparse orig tmpfile;
      Common.command2(spf "diff %s %s" x tmpfile);
    );
  "-parse_view", "   <file>", 
    Common.mk_action_1_arg (fun x -> 
      ignore(Code.parse ~lang:Lang.mark_ocaml_short x);
    );

  (* tangling *)
  "-view_of_orig", "   <file> <key>", 
    Common.mk_action_2_arg (fun x key -> 
      let orig = Web.parse x in
      let view = Web_to_code.view_of_orig key orig in
      let tmpfile = "/tmp/xxx" in
      Code.unparse ~lang:Lang.mark_ocaml view tmpfile;
      tmpfile |> Common.cat |> List.iter pr;
      (*Common.command2(spf "diff %s %s" x tmpfile); *)
    );

  (* weaving *)
  "-to_tex", " <nw file> <defs and uses file>", 
  Common.mk_action_2_arg (fun origfile defs_and_uses_file -> 
    (* todo: parse .aux? *)
    let (d,b,e) = Common2.dbe_of_filename origfile in
    if (e <> "nw")
    then failwith (spf "expect a .nw file not a .%s" e);
    let orig = Web.parse origfile in
    let (defs, uses) = Crossref_code.parse_defs_and_uses defs_and_uses_file in
    (* multi-file support *)
    let orig = Web.expand_sharp_include orig in
    let texfile = Common2.filename_of_dbe (d,b,"tex") in
    Web_to_tex.web_to_tex orig texfile (defs, uses);
  );

  (* superseded by Main.main_action now *)
  "-sync", "   <orig> <view>", 
    Common.mk_action_2_arg (fun origf viewf -> 
      let orig = Web.parse origf in
      let views = Code.parse ~lang:Lang.mark_ocaml viewf in

      let orig' = Sync.sync ~lang:Lang.mark_ocaml     orig views  in

      let tmpfile = "/tmp/xxx" in
      Web.unparse orig' tmpfile;
      Common.command2(spf "diff %s %s" origf tmpfile);
    );
  "-unmark", "   <file>", 
    Common.mk_action_1_arg (fun file -> 

      let xs = Common.cat file in
      let xs = xs |> Common.exclude (fun s ->
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


  "-rename_chunknames", " <origs>", 
  Common.mk_action_n_arg Refactor.rename_chunknames;
  "-rename_chunknames_archi", " <origs and views>", 
  Common.mk_action_n_arg Refactor.rename_chunknames_archi;
  "-merge_files", " <origs>", 
  Common.mk_action_n_arg Refactor.merge_files;

  (* pad's hacks *)
  "-to_noweb", " <orig>", 
  Common.mk_action_1_arg (fun file -> 
    let orig = Web.parse file in
    let orig =
      orig |> List.map (function 
        | Tex xs -> 
          Tex (xs |> List.map (fun s -> 
            match s with
            | _ when s =~ "^%.*" -> "%"
            | _ when s =~ "^[ \t]*\\\\[tln] " -> "%"
            (* less: #include *)
            | _ -> s
          ))
        | x -> x
      )
    in
    Web.unparse orig (file ^ "_noweb.nw")
  );

  "-to_web", " <orig>", 
  Common.mk_action_1_arg (fun file -> 

let unparse_orig_web orig filename =
  Common.with_open_outfile filename (fun (pr_no_nl, _chan) -> 
    let pr s = pr_no_nl (s ^ "\n") in
    orig |> List.iter (function
    | Tex xs -> 
        xs |> List.iter pr;
    | ChunkDef (def, body) -> 
        let start = spf "<<%s>>=" def.chunkdef_key in
        let end_mark = def.chunkdef_end in
        pr start;
        body |> List.iter (function
        | Code s -> 
            pr s
        | ChunkName (s, indent) -> 
            Common2.do_n indent (fun () -> pr_no_nl " ");
            let item = spf "<<%s>>" s in
            pr item;
        );
        pr end_mark;
    );
  )
in

    let orig = Web.parse file in
    let (d,b,e) = Common2.dbe_of_filename file in
    let file2 = Common2.filename_of_dbe (d,b,"w") in
    unparse_orig_web orig file2
  );

]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  let md5sum_in_auxfile = !md5sum_in_auxfile in
  let less_marks = !less_marks in
  let lang = 
    try List.assoc !lang (Lang.lang_table md5sum_in_auxfile)
    with Not_found -> failwith (spf "lang %s not found" !lang)
  in

  match xs with
  (* simple case, one tex.nw file, one view *)
  | [origf;viewf] -> 

      let orig = Web.parse origf in
      let topkey = 
        (* old: Filename.basename viewf *)
        find_topkey_corresponding_to_file orig viewf
      in
      if not (Sys.file_exists viewf)
      then
        let view = Web_to_code.view_of_orig ~topkey orig in
        Code.unparse ~md5sum_in_auxfile ~less_marks ~lang view viewf
      else begin
        (* old: let date1 = Common.filemtime origf in
         *      let date2 = Common.filemtime viewf in
         *)
        (* pr2 (spf "syncing %s and %s with key %s" origf viewf topkey);  *)
        let view = Code.parse ~lang viewf in 
        let orig' = Sync.sync ~lang   orig view in
        let view' = Web_to_code.view_of_orig ~topkey orig' in
        (* regenerate orig and view *)
        if orig <> orig' then begin
          pr2 "orig has been updated";
          Web.unparse orig' origf;
        end;
        if view <> view' then begin
          pr2 "view has been regenerated";
          Code.unparse ~md5sum_in_auxfile ~less_marks ~lang view' viewf;
        end;
      end

  (* many .tex.nw, one view (to be called repeatedely for each view) *)
  | xs when List.length xs > 2 -> 
      let origfs, viewf = 
        match List.rev xs with
        | x::xs -> List.rev xs, x
        | _ -> raise Impossible
      in
      let origs = origfs |> List.map (fun f -> 
        with_error f (fun () -> f, Web.parse f)
      ) in
      let orig = Web.pack_multi origs in
      let topkey = 
        (* old: Filename.basename viewf *)
        find_topkey_corresponding_to_file orig viewf
      in

      if not (Sys.file_exists viewf)
      then 
        let view = Web_to_code.view_of_orig ~topkey orig in
        Code.unparse ~md5sum_in_auxfile ~less_marks ~lang view viewf
      else begin
        (* pr2 (spf "syncing %s" viewf); *)

        with_error viewf (fun () ->
        let view = Code.parse ~lang viewf in 
        let orig' = Sync.sync ~lang  orig view in
        let view' = Web_to_code.view_of_orig  ~topkey orig' in
        (* regenerate orig and view *)
        if view <> view' then begin
          pr2 "view has been regenerated";
          Code.unparse ~md5sum_in_auxfile ~less_marks ~lang view' viewf;
        end;
        let origs' = Web.unpack_multi orig' in
        Common2.zip origs origs' |> List.iter (fun ((f1, orig), (f2, orig')) ->
          if orig <> orig' then begin
            pr2 (spf "orig %s has been updated" f1);
            Web.unparse orig' f1;
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
        (Common.join "|" (List.map fst (Lang.lang_table true))));

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
