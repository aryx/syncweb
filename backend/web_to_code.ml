(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

open Web
open Code

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(* Getting the code from a noweb file,
 * a.k.a. 'tangling' in Knuth's and literate programming terminology.
 * I could call this file tangle.ml, but I always found this terminology
 * confusing.
 * 
 * related:
 *  - notangle from noweb from Norma Ramsey
 *  - nofake Perl script from Christian Lindig
 *  - lipsum from Christian Lindig
 *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
let generate_n_spaces i =
  Common2.repeat " " i |> Common.join ""

let s_of_chunkdef_body xs = 
  xs |> List.map (function 
  | Code s -> s
  | ChunkName (s, i) -> 
      let spaces = generate_n_spaces i in
      spaces ^ (spf "<<%s>>" s)
  ) |> Common2.unlines


(*****************************************************************************)
(* Orig->View  *)
(*****************************************************************************)


(* then see Code.unparse to save the code in a file *)
let web_to_code ~topkey orig = 
  let h = Crossref_chunk.hchunkname_to_body__from_orig orig in

  let rec aux (key, i) = 
    let bodys = 
      try 
        Hashtbl.find h key 
      with Not_found -> 
        failwith (spf "view_of_orig: not able to find the chunkdef of '%s'" key)
    in

    bodys |> List.map (fun body -> 

      let s = s_of_chunkdef_body body in
      let md5sum = Signature.signature_of_string s in

      let body' = 
        body |> List.map (function
        | Code s -> [RegularCode s]
        | ChunkName (s,i) -> 
            aux (s,i)
        ) |> List.flatten
      in

      ChunkCode ({
        chunk_key = key;
        chunk_md5sum = Some md5sum;
        pretty_print = None;
      }, body', i)
    )
  in
  aux (topkey, 0)

let view_of_orig ~topkey a = 
(*  Profiling.profile_code "view_of_orig" (fun () ->  *)
      web_to_code ~topkey a
(*  ) *)
