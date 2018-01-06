
type chunk_xref = {
  mutable prev_def: Web.chunkid option;
  mutable next_def: Web.chunkid option;
  mutable chunk_users: Web.chunkid list;
}

val hchunkname_to_def__from_orig:
  Web.t -> (Web.chunkname, Web.chunkdef) Hashtbl.t
val hchunkid_info__from_orig:
  Web.t -> (Web.chunkid, chunk_xref) Hashtbl.t

val label_of_id: Web.chunkid -> string
