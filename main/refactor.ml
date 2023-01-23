open Common

open Web
open Code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * adhoc scripts to adjust .nw files.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let count_dollar s =
  let cnt = ref 0 in
  for i = 0 to String.length s - 1 do
    if String.get s i =$= '$'
    then incr cnt
  done;
  !cnt

let trim s =
  Str.global_replace (Str.regexp " +$") "" s

(*****************************************************************************)
(* Renaming chunks *)
(*****************************************************************************)

(* less: could make this a generic mapper *)
let rename_chunknames xs =
  let subst_maybe s =
    let s, suffix =
      match s with
      | _ when s =~ "^\\([^(]+\\)\\(([ax][r8][m6])\\)$" ->
        Common.matched2 s 
      | _ when s =~ "^\\([^(]+\\)\\((raspberry pi[12])(arm)\\)$" ->
        Common.matched2 s 

      (* for CompilerGenerator.nw *)
      | _ when s =~ "^\\([^(]+\\)\\((lex)\\)$" ->
        Common.matched2 s 
      | _ when s =~ "^\\([^(]+\\)\\((yacc)\\)$" ->
        Common.matched2 s 

      | _ when s =~ "^\\([^(]+\\) \\(([a-z_0-9]+/.*)\\)$" ->
        let (a, b) = Common.matched2 s in
        a, spf "([[%s]])" b
      | _ -> s, ""
    in
    let s = trim s in
    let res = 
    match s with
    | _ when s =~ "^function \\([a-zA-Z0-9_.]+\\)$" ->
      spf "function [[%s]]" (Common.matched1 s)
    | _ when s =~ "^signature \\([a-zA-Z0-9_.]+\\)$" ->
      spf "signature [[%s]]" (Common.matched1 s)
    | _ when s =~ "^exception \\([a-zA-Z0-9_.]+\\)$" ->
      spf "exception [[%s]]" (Common.matched1 s)
    | _ when s =~ "^constructor \\([a-zA-Z0-9_.]+\\)$" ->
      spf "constructor [[%s]]" (Common.matched1 s)
    | _ when s =~ "^destructor \\([a-zA-Z0-9_]+\\)$" ->
      spf "destructor [[%s]]" (Common.matched1 s)
    | _ when s =~ "^global \\([a-zA-Z0-9_.]+\\)$" ->
      spf "global [[%s]]" (Common.matched1 s)
    | _ when s =~ "^enum \\([a-zA-Z0-9_.]+\\)$" ->
      spf "enum [[%s]]" (Common.matched1 s)
    | _ when s =~ "^type \\([a-zA-Z0-9_.]+\\)$" ->
      spf "type [[%s]]" (Common.matched1 s)
    | _ when s =~ "^struct \\([a-zA-Z0-9_]+\\)$" ->
      spf "struct [[%s]]" (Common.matched1 s)
    | _ when s =~ "^macro \\([a-zA-Z0-9_]+\\)$" ->
      spf "macro [[%s]]" (Common.matched1 s)
    | _ when s =~ "^constant \\([a-zA-Z0-9_.]+\\)$" ->
      spf "constant [[%s]]" (Common.matched1 s)
    | _ when s =~ "^toplevel \\([a-zA-Z0-9_.]+\\)$" ->
      spf "toplevel [[%s]]" (Common.matched1 s)
    | _ when s =~ "^typedef \\([a-zA-Z0-9_.]+\\)$" ->
      spf "typedef [[%s]]" (Common.matched1 s)

    | _ when s =~ "^enum _anon_ \\(.*\\)$" ->
      spf "enum [[_anon_ %s]]" (Common.matched1 s)
    | _ -> s
    in
    res ^ suffix
  in
  xs |> List.iter (fun file ->
    let orig = Web.parse file in
    
    let rec tex_or_chunkdef x =
      match x with
      | Tex xs -> Tex xs
      | ChunkDef (def, ys) ->
          let def = { def with chunkdef_key = subst_maybe def.chunkdef_key } in
          ChunkDef (def, ys |> List.map code_or_chunk)
    and code_or_chunk x =
      match x with
      | Code s -> Code s
      | ChunkName (s, i) -> ChunkName (subst_maybe s, i)
    in
    let orig2 = List.map tex_or_chunkdef orig in
    Web.unparse orig2 file
  )

(*****************************************************************************)
(* Rename chunks to indicate arch specific code (x86) or (arm) *)
(*****************************************************************************)

let rename_chunknames_archi xs =
  let origs, views = xs |> Common.partition_either (fun file ->
    if file =~ ".*.nw$"
    then Left file
    else Right file
  )
  in

  let hchunks = Hashtbl.create 101 in
    
  views |> List.iter (fun file ->
    let view = Code.parse ~lang:Lang.mark_C_short file in

    let rec codetree x =
      match x with
      | RegularCode _ -> ()
      | ChunkCode (info, xs, _indent) ->
        Hashtbl.replace hchunks info.chunk_key true;
        xs |> List.iter codetree
    in
    List.iter codetree view;
    hchunks |> Common.hash_to_list |> List.iter (fun (k, _) -> 
      pr k
    );
  );
  let subst_maybe s =
    if s =~ ".*8[acl]/" 
    || s =~ ".*386"
    || s =~ ".*x86" (* avoid apply on what was already applied *)
    || s = "kernel basic includes"
    || s =~ ".*\\.[chs]$"
    then s
    else
      if Hashtbl.mem hchunks s
      then s ^ "(x86)"
      else s
  in

  origs |> List.iter (fun file ->
    let orig = Web.parse file in
    
    let rec tex_or_chunkdef x =
      match x with
      | Tex xs -> Tex xs
      | ChunkDef (def, ys) ->
          let def = { def with chunkdef_key = subst_maybe def.chunkdef_key } in
          ChunkDef (def, ys |> List.map code_or_chunk)
    and code_or_chunk x =
      match x with
      | Code s -> Code s
      | ChunkName (s, i) -> ChunkName (subst_maybe s, i)
    in
    let orig2 = List.map tex_or_chunkdef orig in
    Web.unparse orig2 file
  )

(*****************************************************************************)
(* Merge files?? *)
(*****************************************************************************)

let merge_files xs =
  let hchunkkey_to_files = Hashtbl.create 101 in
  let htopkeysfile = Hashtbl.create 101 in
  let hfile_to_topkeys = Hashtbl.create 101 in

  (* first pass, find duplicate chunk names in different .nw *)
  xs |> List.iter (fun file ->
    let orig = 
      try
        Web.parse file 
      with exn ->
        failwith (spf "PB with %s, exn = %s" file (Common.exn_to_s exn))
    in

    let rec tex_or_chunkdef x =
      match x with
      | Tex _xs -> ()
      | ChunkDef (def, ys) ->
        let key = def.chunkdef_key in
        let hfiles_of_key = 
          try Hashtbl.find hchunkkey_to_files key
          with Not_found ->
            let h = Hashtbl.create 101 in
            Hashtbl.add hchunkkey_to_files key h;
            h
        in
        Hashtbl.replace hfiles_of_key file true;
        if key =~ ".*\\.ml[i]?$" 
        then begin
          let path = Filename.concat (Filename.dirname file) key in
          if Sys.file_exists path && not (Hashtbl.mem htopkeysfile path)
          then begin 
            Hashtbl.add htopkeysfile path true;
            (* pr2 (spf " %s\\" path); *)
            Hashtbl.add hfile_to_topkeys file key
          end
        end;

        ys |> List.iter code_or_chunk
    and code_or_chunk x =
      match x with
      | Code _s -> ()
      | ChunkName (_s, _i) -> ()
    in
    List.iter tex_or_chunkdef orig
  );

  let lastdir = ref "" in

  (* second pass, rename them *)
  xs |> List.iter (fun file ->
    let dir = Filename.dirname file in
    (* let pr _ = () in (* TODO *)   *)
    if dir <> !lastdir then begin
      pr "";
      pr (spf "\\chapter{[[%s]]}" dir);
      pr "";
      lastdir := dir
    end;

    pr (spf "\\section{[[%s]]}" file);

    (* to have a single topkey entry *)
    let xs = Hashtbl.find_all hfile_to_topkeys file in
    xs |> List.iter (fun topkey ->
      pr (spf "<<%s/%s>>=" dir topkey);
      pr (spf "<<%s>>" topkey);
      pr "@";
      pr ""
    );
    
    let orig = Web.parse file in

    let subst_maybe key =
      try 
        let h = Hashtbl.find hchunkkey_to_files key in
        let files = Common.hashset_to_list h in
        if List.length files > 1
        then key ^ (spf "(%s)" (Filename.basename file))
        else key
      with Not_found -> key
    in

    let rec tex_or_chunkdef x =
      match x with
      | Tex xs -> 
        [Tex (xs |> List.map (fun s ->
          if s =~ "^\\\\section" ||
             s =~ "^\\\\subsection" ||
             s =~ "^%----"
          then s
          else "%%" ^ s
        ))]
      | ChunkDef (def, ys) ->
          (*TODO: detect if even number of $ in which case need
           * add a fake %$ to the end
           *)
          let def = { def with chunkdef_key = subst_maybe def.chunkdef_key } in
          let nbdollars = ys |> List.map (function
            | Code s -> count_dollar s
            | ChunkName (s, _) -> count_dollar s
          ) |> Common2.sum
          in
          [ChunkDef (def, ys |> List.map code_or_chunk)] @
          (if nbdollars mod 2 =|= 1
           then [Tex ["%$"]]
           else []
          )
    and code_or_chunk x =
      match x with
      | Code s -> Code s
      | ChunkName (s, i) -> ChunkName (subst_maybe s, i)
    in
    let orig2 = List.map tex_or_chunkdef orig |> List.flatten in
    Web.unparse orig2 file;

    Common.cat file |> List.iter pr; 
    Sys.command (spf "rm -f %s" file) |> ignore;

    Hashtbl.find_all hfile_to_topkeys file |> List.iter (fun topkey ->
      Sys.command (spf "rm -f %s/%s" dir topkey) |> ignore;
    )
  )
