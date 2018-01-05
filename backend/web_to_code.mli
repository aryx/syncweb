
val web_to_code:
  topkey:Web.chunkname -> Web.t -> Code.t
(* alias *)
val view_of_orig:
  topkey:Web.chunkname -> Web.t -> Code.t

(* used by sync.ml *)
val build_chunk_hash_from_orig:
  Web.t -> (Web.chunkname, Web.code_or_chunk list list) Hashtbl.t
val s_of_chunkdef_body:
  Web.code_or_chunk list -> string
