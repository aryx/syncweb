open Common

open Web

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

(* yet another Parse_info.token_location, 
 * but I don't want to depend on pfff/h_program-lang *)
type loc = { 
  file: Common.filename;
  line: int;
}
(* yet another Entity_code.t,
 * but I don't want to depend on pfff/h_program-lang *)
type entity_kind =
  | Function
  | Global

  | Constant
  | Macro
  | Constructor

  | Type
  | Field

type defs = ((string * entity_kind) * loc) list
type uses = ((string * entity_kind) * loc) list

let debug = ref false

(*****************************************************************************)
(* Parsing  *)
(*****************************************************************************)

(* mostly copy-paste of Entity_code.entity_kind_of_string *)
let kind_of_string_opt = function
  | "Function" -> Some Function
  | "Global" -> Some Global

  | "Constant" -> Some Constant
  | "Macro" -> Some Macro
  | "Constructor" -> Some Constructor

  | "Type" -> Some Type
  | "Field" -> Some Field

  | "Prototype" 
  | "GlobalExtern"
    -> None
  | s -> failwith (spf "unsupported kind: %s" s)
(*
  | "Class" -> Some Class
  | "Module" -> Some Module
  | "TopStmts" -> Some TopStmts
  | "Method" -> Some Method
  | "ClassConstant" -> Some ClassConstant
  | "File" -> Some File
  | "Dir" -> Some Dir
  | "MultiDirs" -> Some MultiDirs
  | "Exception" -> Some Exception
*)
(*
  | _ when s =~ "Other:\\(.*\\)" -> Other (Common.matched1 s)
*)
  | s -> None

(* the data is generated by ../indexer/index_pfff.ml *)
let parse_defs_and_uses file =
  let defs = ref [] in
  let uses = ref [] in
  Common.cat file |> List.iter (fun s ->
    let xs = Str.split_delim (Str.regexp ":") s in
    match xs with
    | ["DEF";kind_str;file;line;name] ->
      let kind_opt = kind_of_string_opt kind_str in
      kind_opt |> Common.do_option (fun kind ->
        defs |> Common.push ((name, kind),
                           {file;line = int_of_string line})
      )
    | ["USE";kind_str;file;line;name] ->
      let kind_opt = kind_of_string_opt kind_str in
      kind_opt |> Common.do_option (fun kind ->
        uses |> Common.push ((name, kind), 
                           {file;line = int_of_string line})
      )
    | _ -> failwith (spf "unrecognized line in defs and uses file: %s" s)
  );
  !defs, !uses

(*****************************************************************************)
(* Chunkid <-> loc list  *)
(*****************************************************************************)

(*****************************************************************************)
(* Chunkid -> defs * uses *)
(*****************************************************************************)
let hdefs_and_uses_of_chunkid__from_orig orig (defs, uses) =
  let hchunkname_to_defs = Crossref_chunk.hchunkname_to_defs__from_orig orig in

  let hresult = Hashtbl.create 101 in

  (* step1: get the list of files mentioned in defs and uses, so we know
   * all the toplevel file chunks
   *)
  let files =
    ((defs |> List.map (fun ((_, _), x) -> x.file))@
     (uses |> List.map (fun ((_, _), x) -> x.file))) |> Common2.uniq
  in

  (* step2: tangle the toplevel file chunks (e.g., mk/main.c) while
   * remembering which LOC correspond to which chunkid
   *)
  (* use Hashtbl.find_all property *)
  let hchunkid_to_locs = Hashtbl.create 101 in
  files |> List.iter (fun file ->
    let loc = ref 1 in
    (* similar to web_to_code.ml *)
    let rec aux key =
      let defs = Hashtbl.find_all hchunkname_to_defs key |> List.rev in
      incr loc (* the s: *);
      defs |> List.iter (fun (def, body) -> 
        (* this assumes you are using -less_marks *)
        let id = def.chunkdef_id in
        body |> List.iter (function
          | Code _ -> 
            Hashtbl.add hchunkid_to_locs id ({ file; line = !loc});
            if !debug 
            then pr (spf "id = %d (name = %s), loc = %s:%d"
                       id key file !loc);
            incr loc
          | ChunkName (key, _indent) ->
            aux key
        );
      incr loc (* the e: or x: *)
      );
    in
    aux file
  );

  (* step3: create hashtbl to go from file x LOC to defs and uses *)
  let hloc_to_defs_uses = 
    ((defs |> List.map (fun ((a, b), loc) -> loc, Left ((a, b), loc))) @
     (uses |> List.map (fun ((a, b), loc) -> loc, Right ((a, b), loc)))
    ) |> Common.hash_of_list
  in

  (* step4: iterate over all LOC for a chunkid and accumulate the defs
   * and uses there
   *)
  hchunkid_to_locs |> Hashtbl.iter (fun id _ ->
    let locs = Hashtbl.find_all hchunkid_to_locs id in
    let defs_uses = 
      locs |> Common.map_filter (fun loc ->
        Common2.hfind_option loc hloc_to_defs_uses
      )
    in
    let defs, uses = Common.partition_either (fun x -> x) defs_uses in
    Hashtbl.add hresult id (defs, uses)
  );
  hresult
