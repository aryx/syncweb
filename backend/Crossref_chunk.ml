open Common

open Web

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(* ?? *)


(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

(* the goal is that given a chunkid, we can know the previous and next
 * definitions sharing the same chunkname and the users of the chunk.
 *)
type chunk_xref = {
  mutable prev_def: chunkid option;
  mutable next_def: chunkid option;
  mutable chunk_users: chunkid list;
}

(*****************************************************************************)
(* Chunkname to X  *)
(*****************************************************************************)
(* for web_to_code.ml and sync.ml *)
let hchunkname_to_body__from_orig orig = 
  let h = Hashtbl.create 101 in
    orig |> List.iter (function
    | Tex _xs -> ()
    | ChunkDef (def, body) -> 
        let key = def.chunkdef_key in
        Common2_.hupdate_default key (fun x -> x @ [body]) (fun()->[]) h;
    );
  h

(* for crossref_code.ml *)
let hchunkname_to_defs__from_orig orig =
  (* use the Hashtbl.find_all *)
  let h = Hashtbl.create 101 in
  let aux orig = 
    orig |> List.iter (function
      | Tex _xs -> ()
      | ChunkDef (def, body) -> 
        let key = def.chunkdef_key in
        Hashtbl.add h key (def, body)
    );
  in
  aux orig;
  h

(* for web_to_tex.ml *)
let hchunkname_to_def__from_orig orig =
  let h = Hashtbl.create 101 in
  let aux orig = 
    orig |> List.iter (function
      | Tex _xs -> ()
      | ChunkDef (def, _body) -> 
        let key = def.chunkdef_key in
        (* we refer to the first one *)
        if Hashtbl.mem h key
        then ()
        else Hashtbl.add h key def
    );
  in
  aux orig;
  h

(*****************************************************************************)
(* Chunkid to X  *)
(*****************************************************************************)

let hchunkid_info__from_orig orig =
  let hchunkid_info = Hashtbl.create 101 in
  let hkey_to_defs = hchunkname_to_defs__from_orig orig in

  (* first pass *)
  orig |> List.iter (function
    | Tex _ -> ()
    | ChunkDef (def, _body) ->
      let info = {
        prev_def = None;
        next_def = None;
        chunk_users = [];
      } in
      Hashtbl.add hchunkid_info def.chunkdef_id info
  );
  let hlast_key_to_chunk = Hashtbl.create 101 in

  (* second pass *)
  let rec tex_or_chunkdef = function
    | Tex _ -> ()
    | ChunkDef (def, body) ->
      let key = def.chunkdef_key in
      let id = def.chunkdef_id in
      let info = Hashtbl.find hchunkid_info id in
      let prev_opt = Common2_.hfind_option key hlast_key_to_chunk in
      Hashtbl.replace hlast_key_to_chunk key id;
      info.prev_def <- prev_opt;
      prev_opt |> Option.iter (fun previd ->
        let info_prev = Hashtbl.find hchunkid_info previd in
        info_prev.next_def <- Some id
      );
      body |> List.iter (code_or_chunk id)

  and code_or_chunk id_enclosing_chunk = function
    | Code _ -> ()
    | ChunkName (key, _indent) ->
      (* todo: should update the uses of all ids ... use a
       * Hashtbl.find_all? 
       *)
      let defs = Hashtbl.find_all hkey_to_defs key in
      if defs =*= []
      then failwith (spf "could not find key for %s" key);
      defs |> List.iter (fun (def, _body) ->
        let id = def.chunkdef_id in
        let info = Hashtbl.find hchunkid_info id in
        info.chunk_users <- id_enclosing_chunk::info.chunk_users
      );
  in
  List.iter tex_or_chunkdef orig;
  hchunkid_info

let label_of_id id =
  spf "NW%d" id
