(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST for the .nw file.
 *
 * Web.t below used to be called Engine.orig, and Code.t Engine.view, so
 * many of the comments below refer to this old naming scheme.
 * This was before I decided that syncweb could also generate
 * the .tex (bypassing completely noweb).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* note: in fact it's not really a list but more an alternating list. We can
 * not have a Tex::Tex::_ normally. A Tex is always followed by a Chunkdef.
 * But is it worthwhile to be more precise? hmmm in fact you can have 
 * at least ChunkDef::ChunkDef::_ so probably not worthwhile.
 *)
type t = tex_or_chunkdef list

 and tex_or_chunkdef =
    (* this can contain some #include (pad's hack) and noweb quotes [[]] *)
   | Tex of tex_string list (* why a list? because each elt is a line *)
   | ChunkDef of chunkdef * code_or_chunk list

   and chunkdef = {
    chunkdef_key: chunkname;
    chunkdef_end: string; (* specific string *)
    (* this is used in web_to_tex to store in external hashtbl additional
     * information about a chunk
     *)
    chunkdef_id: chunkid;
   }
   and code_or_chunk =
     | Code of string
     | ChunkName of chunkname * int (* indentation *)
  (* Those strings can contain noweb quotes ([[ ]]), but they are
   * not parsed here. See Web_to_tex.tex_string instead.
   *)
  and tex_string = string 
  and chunkname = tex_string
  and chunkid = int
[@@deriving show { with_path = false} ]

(*****************************************************************************)
(* Invariants  *)
(*****************************************************************************)

(* let check_view v = 
 * 
 * can not have multiple chunkname with same key and different ident 
 * and not consecutive.
 * 
 *)

(* let check_orig x = 
*)


(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let (==~) s re =
    Str.string_match re s 0

(*****************************************************************************)
(* Parser  *)
(*****************************************************************************)

(* First version: do as in nofake, very line-oriented and assume
 * a few things:
 *   - <<xx>>= and @ in first column and alone and on single line
 *   - <<yy>> inside chunk are singular, possibly with a prefix
 *     code and postfix code, but there is only one <<yy>> per line.
 * 
 * Have some limitations, but simpler, and in practice probably good enough.
 * 
 * less: allow optional '@' when have a Start1 after another Start1?
 *)

type mark1 = Regular1 | Start1 | End1 

(* less: use pcre so can do .*? *)
let regexp_chunkdef = Str.regexp "^<<\\(.*\\)>>=[ \t]*$" 

let regexp_chunkdef_end = Str.regexp "^@[ \t]*$"

(* (.*[^\@]* )<<([^<>@]+)>>(. * ) *)
(* todo: more flexible ? *)
let regexp_chunk_ref = Str.regexp 
  "\\([ \t]*\\)<<\\(.*\\)>>[ \t]*$"

let key_and_index_chunk_ref_string s = 
  if s ==~ regexp_chunk_ref
  then
    let (space_or_tabs, key) = matched2 s in
    (* todo? handle tabs ? *)
    key, String.length space_or_tabs
  else failwith "not a chunk_ref string"

let key_of_chunckdef_string s = 
  if s ==~ regexp_chunkdef
  then matched1 s
  else failwith "not a chunkdef string"


let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z

let cnt_id = ref 0

let parse (file : Fpath.t) : t = 
  Logs.info (fun m -> m "parsing %s" !!file);
  let xs = UFile.cat file in 
  let xs' = xs |> List_.index_list_1 |> List.map (fun (s, i) -> 
    s, i,
    (match s with
    | _ when s ==~ regexp_chunkdef_end -> End1
    | _ when s ==~ regexp_chunkdef     -> Start1
    | _ -> Regular1
    )
  )
  in
  (* todo: more flexible *)
  let process_body ys =
    ys |> List.map (fun s -> 
      if s ==~ regexp_chunk_ref
      then
        let (key, indent) = key_and_index_chunk_ref_string s in
        ChunkName (key, indent)
      else Code s
    )
  in
  let rec agglomerate xs = 
    match xs with
    | [] -> []
    | x::xs -> 
        let line = snd3 x in 

        (match thd3 x with
        | Regular1 -> 
            let (regs, rest) = List_.span (fun x -> thd3 x =*= Regular1) xs in
            let item = Tex (fst3 x::(List.map fst3 regs)) in
            item::agglomerate rest
        | Start1 -> 
            (try 
              let (body, endmark, rest) = 
                Common2.split_when (fun x -> thd3 x =*= End1) xs
              in
              if (not (body |> List.for_all (fun x -> thd3 x =*= Regular1)))
              then failwith 
                (spf "line %d: body of chunkdef contains other chunkdef" line);

              let body' = List.map fst3 body in
              incr cnt_id;
              let item = ChunkDef ({
                chunkdef_key = key_of_chunckdef_string (fst3 x);
                chunkdef_end = fst3 endmark;
                chunkdef_id = !cnt_id;
              }, process_body body') in
              item::agglomerate rest
            with Not_found -> 
              failwith (spf "no end mark found, at line %d" line)
            )

        | End1 -> failwith (spf "line %d: a end mark without a start" line)
        )
  in
  agglomerate xs'
[@@profiling]

(*****************************************************************************)
(* Unparser *)
(*****************************************************************************)

let unparse (orig : t) (filename : Fpath.t) : unit =
  UFile.with_open_out filename (fun (pr_no_nl, _chan) -> 
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
            Common2_.do_n indent (fun () -> pr_no_nl " ");
            let item = spf "<<%s>>" s in
            pr item;
        );
        pr end_mark;
    );
  )
[@@profiling]

(*****************************************************************************)
(* Multi file support *)
(*****************************************************************************)

let expand_sharp_include (orig : t) : t =
  Logs.info (fun m -> m "expanding #include");
  orig |> List.map (function
    | Tex xs ->
      xs |> List.map (fun s ->
        match s with
        | _ when s =~ "#include +\"\\(.*\\.nw\\)\"" ->
          let file = Common.matched1 s in
          (* TODO: shoud recurse and expand_sharp_include in this orig too! *)
          let orig = parse (Fpath.v file) in
          orig
        | _ -> [Tex [s]]
        ) |> List.flatten
    | ChunkDef (def, xs) ->
      [ChunkDef (def, xs)]
  ) |> List.flatten


(* For the moment the multi file support is really a hack. I just 
 * abuse the Tex constructor to remember that a serie of tex_or_chunkdef 
 * belongs to a file. This hack allows to use 'sync' as-is, without any
 * additional coding, for free.
 * 
 * Assumption: the file list are given in a good order, the order of 
 * the #include inside the .nw files. Moreover it assumes the appended chunks
 * are defined in the good order too. As most of the time I use the multi
 * file in a very basic way, just to split a big .nw. this is not a problem
 * I think.
 * 
 * less? could introduce a MultiFileHack of Common.filename constructor
 * instead of abusing Tex
 *)

let pack_multi (xs : (Fpath.t * t) list) : t = 
  xs |> List.map (fun (file, ys) -> 
    Tex (["MULTIFILE:" ^ !!file])::ys
  ) |> List.flatten


let unpack_multi (orig : t) : (Fpath.t * t) list =
  let (pre, groups) = 
    Common2_.group_by_pre (fun x ->
      match x with
      | Tex [s] when s =~ "MULTIFILE:.*" -> true
      | _ -> false
    ) orig
  in
  if not (List_.null pre)
  then failwith "could not find a MULTIFILE mark in packed orig, weird";

  groups |> List.map (fun (x, xs) ->
    match x with
    | Tex [s] when s =~ "MULTIFILE:\\(.*\\)$" ->
        Fpath.v (Common.matched1 s), xs
    | _ -> raise Impossible
  )

