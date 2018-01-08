
type loc = {
  file: Common.filename;
  line: int;
}

type entity_kind =
  | Function
  | Global
  | Constant
  | Macro

type defs = ((string * entity_kind) * loc) list
type uses = ((string * entity_kind) * loc) list

val parse_defs_and_uses: 
  Common.filename -> defs * uses

val hdefs_and_uses_of_chunkid__from_orig:
  Web.t -> (defs * uses) ->
  (Web.chunkid, defs * uses) Hashtbl.t
