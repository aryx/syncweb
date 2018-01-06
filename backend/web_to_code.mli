
val web_to_code:
  topkey:Web.chunkname -> Web.t -> Code.t
(* alias *)
val view_of_orig:
  topkey:Web.chunkname -> Web.t -> Code.t

(* used by sync.ml *)
val s_of_chunkdef_body:
  Web.code_or_chunk list -> string
