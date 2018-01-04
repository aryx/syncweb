
(* usually a .nw file *)
type t = tex_or_chunkdef list
  and tex_or_chunkdef =
    | Tex      of tex_string list
    | ChunkDef of chunkdef * code_or_chunk list

    and chunkdef = {
      chunkdef_key: tex_string;
      chunkdef_end: string; (* usually just '@' *)
    }
    and code_or_chunk =
      | Code of string
      | ChunkName of tex_string * int (* indentation *)
  (* Those strings can contain noweb quotes ([[ ]]), but they are
   * not parsed here. See Web_to_tex.texstring instead.
   *)
  and tex_string = string 

val parse: Common.filename -> t

val unparse: t -> Common.filename -> unit

(* multi file support *)
val pack_multi: 
  (Common.filename * t) list -> t
val unpack_multi: 
  t -> (Common.filename * t) list
