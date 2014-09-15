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
 * todo: 
 *  - optimize make sync when have many files, cache in a .marshall
 *    the parsing of the .tex.nw? or use hashtbl instead of list for faster
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
    pr2 (spf "PB while was processing %s" file);
    raise e

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

open Engine
(* allows to have multiple filenames with the same name but different dir *)
let find_topkey_corresponding_to_file orig viewf =
  (* old: Filename.basename viewf *)
  let base = Filename.basename viewf in
  let defs = orig +> Common.map_filter (function
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
    let revdir = Filename.dirname viewf +> Common.split "/" +> List.rev in
    let candidates = 
      (x::y::ys) +> List.map (fun file ->
        Filename.dirname file +> Common.split "/" +> List.rev,
        file
       )
    in
    let err () = 
      failwith (spf "too many matching topkeys for %s (%s)" viewf
                  ((x::y::ys) +> Common.join ", "))
    in
    let rec aux revdir candidates =
      match revdir with
      | [] -> err ()
      | x::xs ->
        let same_top_revdir_candidates =
          candidates +> Common.map_filter (fun (revdir, fullfile) ->
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

(* syncweb does not like tabs *)
let untabify s =
  Str.global_substitute (Str.regexp "^\\([\t]+\\)") (fun _wholestr ->
    let substr = Str.matched_string s in
    let n = String.length substr in
    Common2.n_space (4 * n)
  ) s

(* Help to create a first draft, to LPize a big set of files.
 * See now pfff -lpize which supports fine grained split of entities
 * (and also untabification) by actually parsing the source code files.
 *)
let lpize file =
  let files = Common.cat file in
  (* sanity check *)
  let group_by_basename =
    files +> List.map (fun file -> Filename.basename file, file)
      +> Common.group_assoc_bykey_eff
  in
  group_by_basename +> List.iter (fun (base, xs) ->
    if List.length xs > 1
    then failwith (spf "multiple files with same name: %s" 
                     (xs +> Common.join "\n"))
  );

  let current_topdir = ref "" in
  files +> List.iter (fun file ->
    let xs = Common.split "/" file in
    let hd = List.hd xs in
    if hd <> !current_topdir
    then begin
      pr (spf "\\section{[[%s/]]}" hd);
      pr "";
      current_topdir := hd;
    end;

      pr (spf "\\subsection*{[[%s]]}" file);
      pr "";
    
    let base = Filename.basename file in
    pr (spf "<<%s>>=" base);
    Common.cat file +> List.iter (fun s ->
      pr (untabify s);
    );
    pr "@";
    pr "";
    pr "";

    (* for the initial 'make sync' to work *)
    (* Sys.command (spf "rm -f %s" file) +> ignore; *)
  )

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
  "-lpize", " <file>",
  Common.mk_action_1_arg lpize;
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
      let topkey = find_topkey_corresponding_to_file orig viewf in
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
      let topkey = find_topkey_corresponding_to_file orig viewf in

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
