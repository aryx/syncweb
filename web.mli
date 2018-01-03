
(* usually a .nw file *)
type t = tex_or_chunkdef list
  and tex_or_chunkdef =
    | Tex      of string list
    | ChunkDef of chunkdef * code_or_chunk list

    and chunkdef = {
      chunkdef_key: string;
      chunkdef_end: string;
    }
    and code_or_chunk =
      | Code of string
      | ChunkName of string * int (* indentation *)

val parse: Common.filename -> t

val unparse: t -> Common.filename -> unit

(* multi file support *)
val pack_multi: 
  (Common.filename * t) list -> t
val unpack_multi: 
  t -> (Common.filename * t) list
