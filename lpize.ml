open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See now pfff -lpize which supports fine-grained split of entities
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* syncweb does not like tabs *)
let untabify s =
  Str.global_substitute (Str.regexp "^\\([\t]+\\)") (fun _wholestr ->
    let substr = Str.matched_string s in
    let n = String.length substr in
    Common2.n_space (4 * n)
  ) s


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* Help to create a first draft, to LPize a big set of files.
 * See now pfff -lpize which supports fine grained split of entities
 * (and also untabification) by actually parsing the source code files.
 *)
let lpize file =
  let files = Common.cat file |> Common.exclude (fun s -> s =~ "^[ \t]*$") in
  (* sanity check *)
  let group_by_basename =
    files +> List.map (fun file -> Filename.basename file, file)
      +> Common.group_assoc_bykey_eff
  in
  group_by_basename +> List.iter (fun (base, xs) ->
    if List.length xs > 1
    then pr2 (spf "multiple files with same name: %s" 
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
    
    let base = (*Filename.basename*) file in
    pr (spf "<<%s>>=" base);
    Common.cat file +> List.iter (fun s ->
      pr (untabify s);
    );
    pr "@";
    pr "";
    pr "";

    (* for the initial 'make sync' to work *)
    Sys.command (spf "rm -f %s" file) +> ignore;
  )

