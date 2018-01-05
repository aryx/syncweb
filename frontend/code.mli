
(* usually a .ml or .mli file *)
type t = codetree list
  and codetree = 
    | RegularCode of string
    | ChunkCode of 
        chunk_info * 
        codetree list (* have adjusted indentation *) *
        int (* indentation, local *)

     and chunk_info = { 
       chunk_key: string;
       (* the md5sum can be in the view or in .md5sum_... aux file *)
       chunk_md5sum: string option;
       mutable pretty_print: position option;
     }
     (* work with the -less_marks flag *)
     and position = First (* s: *) | Middle (* x: *) | Last (* e: *)

(* may also parse the .md5sum_xxx file if it exists *)
val parse: lang:Lang.mark_language -> Common.filename -> t

val unparse: 
  ?md5sum_in_auxfile:bool ->
  ?less_marks:bool ->
  lang:Lang.mark_language -> t -> Common.filename -> unit
