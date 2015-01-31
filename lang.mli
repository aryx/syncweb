
type mark_language = {
  (* return (space, key, md5sum option) *)
  parse_mark_start: string -> (string * string * string option) option;
  (* return (space, key option) *)
  parse_mark_end: string -> (string * string option) option;

  unparse_mark_start: key:string -> md5:string option -> string;
  unparse_mark_end: key:string -> string;

  (* works with less_marks flag *)
  parse_mark_startend: string -> (string * string * string option) option;
  unparse_mark_startend: key:string -> md5:string option -> string;
}

val lang_table : (string, mark_language) Common.assoc

(* for testing *)

val mark_ocaml_short: mark_language
val mark_ocaml: mark_language
val mark_C_short: mark_language
