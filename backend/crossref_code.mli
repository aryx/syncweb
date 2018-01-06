
type loc = { 
  file: Common.filename;
  line: int;
}

type def_kind =
  | Function

type use_kind =
  | Call

type defs = (loc * string * def_kind) list
type uses = (loc * string * use_kind) list

val parse_defs_and_uses: 
  Common.filename -> defs * uses

val hdefs_and_uses_of_chunkid__from_orig:
  Web.t -> (defs * uses) ->
  (Web.chunkid, defs * uses) Hashtbl.t
