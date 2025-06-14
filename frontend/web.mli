
(* usually a .nw file *)
type t = tex_or_chunkdef list
  and tex_or_chunkdef =
    (* this can contain some #include (pad's hack) and noweb quotes [[]] *)
    | Tex      of tex_string list
    | ChunkDef of chunkdef * code_or_chunk list

    and chunkdef = {
      chunkdef_key: chunkname;
      chunkdef_end: string; (* usually just '@' *)
      (* this is used in web_to_tex to store in external hashtbl additional
       * information about a chunk
       *)
      chunkdef_id: chunkid;
    }
    and code_or_chunk =
      | Code of string
      | ChunkName of chunkname * int (* indentation *)
  (* Those strings can contain noweb quotes ([[ ]]), but they are
   * not parsed here. See Web_to_tex.texstring instead.
   *)
  and tex_string = string 
  and chunkname = tex_string
  and chunkid = int

val parse: Fpath.t -> t

val unparse: t -> string (* Common.filename *) -> unit

(* multi file support for weaving *)
val expand_sharp_include: 
  t -> t

(* multi file support for sync *)
val pack_multi: 
  (string (* Common.filename *) * t) list -> t
val unpack_multi: 
  t -> (string (* Common.filename *) * t) list
