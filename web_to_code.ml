(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

open Web
open Code

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * AKA 'tangle' in Knuth's and literate programming terminology.
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

let build_chunk_hash_from_orig orig = 
  let h = Hashtbl.create 101 in
    orig |> List.iter (function
    | Tex xs -> ()
    | ChunkDef (def, body) -> 
        let key = def.chunkdef_key in
        Common2.hupdate_default key (fun x -> x @ [body]) (fun()->[]) h;
    );
  h

(* then see Code.unparse to save the code in a file *)
let web_to_code ~topkey orig = 
  let h = build_chunk_hash_from_orig orig in

  let rec aux (key,i) = 
    let bodys = 
      try 
        Hashtbl.find h key 
      with Not_found -> 
        failwith (spf "view_of_orig: not able to find the chunkdef of '%s'" key)
    in

    bodys |> List.map (fun body -> 

      let s = s_of_chunkdef_body body in
      let md5sum = Common2.md5sum_of_string s in

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

let view_of_orig = web_to_code
