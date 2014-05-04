
(* usually a .nw file *)
type orig = tex_or_chunkdef list
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

(* usually a .ml or .mli file *)
type view = codetree list
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
     (* work with the less_marks flag *)
     and position = First | Middle | Last


(* main entry *)
val sync: lang:Lang.mark_language -> orig -> view -> orig

(* subsystems *)

val parse_orig: Common.filename -> orig
(* may also parse the .md5sum_xxx file if it exists *)
val parse_view: lang:Lang.mark_language -> Common.filename -> view

val unparse_orig: 
  orig -> Common.filename -> unit
val unparse_view: 
  ?md5sum_in_auxfile:bool ->
  ?less_marks:bool ->
  lang:Lang.mark_language -> view -> Common.filename -> unit

val view_of_orig:
  topkey:string -> orig -> view

(* multi file support *)
val pack_multi_orig: 
  (Common.filename * orig) list -> orig
val unpack_multi_orig: 
  orig -> (Common.filename * orig) list

