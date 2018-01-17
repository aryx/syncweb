
type loc = {
  file: Common.filename; (* a relative (readable) path usually *)
  line: int;
}

val string_of_loc: loc -> string

type entity_kind =
  | Function
  | Global

  | Constant
  | Macro
  | Constructor

  | Type (* include struct, union, enum *)
  | Field

  | Typedef
  | Structdef

  | Exception

  | Other of string (* e.g., token category, grammar rule, etc *)

type defs = ((string * entity_kind) * loc) list
type uses = ((string * entity_kind) * loc) list

val parse_defs_and_uses: 
  Common.filename -> defs * uses

val hdefs_and_uses_of_chunkid__from_orig:
  Web.t -> (defs * uses) ->
  (Web.chunkid, defs * uses) Hashtbl.t

(* you can use Hashtbl.find_all on the returned hashtbl. If a string
 * has multiple matching defs then you need a way to disambiguate 
 *)
val hchunkid_of_def__from_orig:
  Web.t -> defs ->
  (string, (entity_kind * loc) * Web.chunkid) Hashtbl.t
  
