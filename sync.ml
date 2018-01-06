(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

open Web
open Code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * history: I started by defining types for orig and view that describes
 * how I want the two formats. Basically a list of stuff (aka chunks in
 * LP terminology) . Then I wrote parse_orig, parse_view, the unparser,
 * and then view_of_orig and orig_of_view. Then finally I wrote sync.
 * 
 * Later:
 *  - the md5sum marks can be in an auxillary file
 * 
 * note: I took inspirations from nofake. Fun to see how Perl is quite
 * good. The nofake program is very short. But Lindig does multiple
 * things at the same time: parsing, building the hash, etc. 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type orig = Web.t
type view = Code.t

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let rec uniq_agglomerate_chunkname xs = 
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> 
      (match x, y with
      | ChunkName (k1, i1), ChunkName(k2, i2) -> 
          if k1 = k2
          then begin
            assert (i1 = i2);
            uniq_agglomerate_chunkname (y::xs);
          end else
            x::(uniq_agglomerate_chunkname (y::xs))
      | _ -> x::(uniq_agglomerate_chunkname (y::xs))
      )

(* can not necessarily know the order, in which order the chunkdefs
 * were put, but can reconstruct a hash with the def of each chunk 
 *)
let build_chunk_hash_from_views views =
  let h = Hashtbl.create 101 in
  let rec aux view = 
    match view with
    | ChunkCode (x, body, i) -> 
        let key = x.chunk_key in
        let md5sum = x.chunk_md5sum in
        
        let body' = 
          body |> List.map (* and side effect *) (fun x -> 
            match x with
            | ChunkCode (y, _body, j) -> 
                aux x;
                ChunkName (y.chunk_key, j)
            | RegularCode s -> 
                Code s
          )
        in
        (* bugfix: some nested chunks can be defined in multiple parts,
         * e.g. <<part1>> can be defined by multiple <<part1>>=, then
         * when expanded, we dont want to return a serie of <<part1>>
         * as in the following:
         * [ChunkName ("type", 0); Code "let foo x = 1"; Code "";
         * Code "let bar y = 2"; Code ""; 
         * ChunkName ("part1", 0);
         * ChunkName ("part1", 0); 
         * ChunkName ("part1", 0); 
         * ChunkName ("part2", 0)]
         *)
        let body'' = uniq_agglomerate_chunkname body' in

        Common2.hupdate_default key (fun x -> x @ [md5sum, body'']) 
          (fun()->[]) h;
        
    | RegularCode s -> 
        failwith ("code without enclosing chunk: " ^ s)
  in
  views |> List.iter aux;
  h

(*****************************************************************************)
(* Diff  *)
(*****************************************************************************)

let show_orig_view ?(force_display=false)key s_orig s_view = 
  pr2 ("DIFF for: " ^ key);
  if (Common2.nblines s_orig > 5 || Common2.nblines s_view > 5) && 
     not force_display 
  then 
    ()
  else begin
    pr2 "<<<<<<< orig <<<<<<<<";
    Common2.pr2_no_nl s_orig;
    pr2 "====================";
    Common2.pr2_no_nl s_view;
    pr2 ">>>>>>> view >>>>>>>>";
  end
 
let show_diff stra strb = 
  let tmpa = "/tmp/a" in
  let tmpb = "/tmp/b" in
  Common.write_file ~file:tmpa stra;
  Common.write_file ~file:tmpb strb;
  Common.command2 (spf "diff -u %s %s" tmpa tmpb);
  ()

(*****************************************************************************)
(* Merger  *)
(*****************************************************************************)

(* When this function is called ? when a chunk body_orig was not found. 
 * Maybe this orig was modified, Maybe the corresponding view was modified. 
 * Maybe a new chunk was inserted, or modified and moved around.
 * 
 * This function is supposed to return a set of view_elems that is
 * safe to "sync" with body_orig. For instance we may not want to return
 * the next elem in view_elems because maybe it is equal to the 
 * next elem in orig_elems. It is ok to return an empty list.
 * 
 * pre: body_orig can not be in view_elems. but it should be the
 * first in orig_elems.
 *)
let candidates_against_orig body_orig view_elems orig_elems =
  view_elems


(* Pierce with his lenses takes also the original view, but we instead use the
 * md5sum in the view as a way to access to the original version of orig.
 *)
let sync ~lang orig views = 

  let h = build_chunk_hash_from_views views in
  let chunks = h |> Common.hash_to_list |> List.map (fun (k, v) -> k, ref v) in
  let h_view = Common.hash_of_list chunks in


  let h = Crossref_chunk.hchunkname_to_body__from_orig orig in
  let chunks = h |> Common.hash_to_list |> List.map (fun (k, v) -> k, ref v) in
  let h_orig = Common.hash_of_list chunks in

  (* we explore the orig in the original order *)
  let orig' = 
    orig |> List.map (function
    | Tex s -> 
        Tex s
    | ChunkDef (def, body_orig) -> 
        let key = def.chunkdef_key in
        
        (match Common2.hfind_option key h_view with
        | None -> 
            (* Case1: new chunk in orig *)
            ChunkDef (def, body_orig)

            (* need to do the following ?
             *     aref_orig := Common.remove_first body_orig !aref_orig; ??
             * no, not needed cos there is no key anyway in h_view.
             *)

        | Some aref_view -> 
            
            (match !aref_view with
            | [] -> 
                (* Case2: old chunk in orig deleted *)
                let s_orig = Web_to_code.s_of_chunkdef_body body_orig in
                
                pr2 ("a chunk has been deleted or moved for: " ^ key);
                pr2 "<<<<<<< orig <<<<<<<<";
                Common2.pr2_no_nl s_orig;
                pr2 "====================";
                if (Common2.y_or_no "keep the one in orig?")
                then ChunkDef (def, body_orig)
                else failwith "stopped"
                
            | x::xs -> 

                (* no need try here *)
                let aref_orig = Hashtbl.find h_orig key in 
                
                (try 
                    (* Case3: equal chunk *)
                    let elem_view = 
                      !aref_view |> List.find (fun (md5, body_view) -> 
                        (* bugfix: have written body_view = body_view :) 
                         * type system can not catch such bugs :( 
                         *)
                        body_orig = body_view
                      )
                    in
                    aref_orig := Common2.remove_first body_orig !aref_orig;
                    aref_view := Common2.remove_first elem_view !aref_view;
                    ChunkDef (def, body_orig)
                  
                 with Not_found -> 
                   
                   (* maybe someone inserted a new append-chunk, and we
                    * would not like that with a simple shift the user 
                    * could be forced to resynchronize and confirm for all the
                    * other parts. So instead try to better match
                    * chunk together.
                    *)
                    let candidates = 
                      candidates_against_orig body_orig !aref_view !aref_orig
                    in
                    (match candidates with
                    | [] -> 
                        (* Case1bis: new chunk in orig ? *)
                        aref_orig := Common2.remove_first body_orig !aref_orig;
                        ChunkDef (def, body_orig)

                    | elem_view::_xs -> 
                        (* case4: multiple possible reasons.
                        *)

                        aref_orig := Common2.remove_first body_orig !aref_orig;
                        aref_view := Common2.remove_first elem_view !aref_view;
                    
                        let (md5sum_orig_in_view_opt, body_view) = elem_view in

                        let md5sum_past = 
                          match md5sum_orig_in_view_opt with
                          | None -> 
                              failwith (spf "TODO: didnt find the md5sum in %s"
                                          (Common.dump elem_view))
                          | Some s -> s
                        in

                        let s_orig = Web_to_code.s_of_chunkdef_body body_orig in
                        let s_view = Web_to_code.s_of_chunkdef_body body_view in

                        let md5sum_orig = Common2.md5sum_of_string s_orig in
                        let md5sum_view = Common2.md5sum_of_string s_view in

                        show_orig_view key s_orig s_view;

                        pr2 "orig          view";
                        (* ask choice or merge *)

                        let first_heuristic = 
                          match () with
                          | _ when md5sum_past =$= md5sum_orig -> 
                              show_diff s_orig s_view;
                              if (Common2.y_or_no 
                                    "        <---- changed?")
                              then Some body_view
                              else None
                                
                          | _ when md5sum_past =$= md5sum_view -> 
                              show_diff s_view s_orig;
                              if (Common2.y_or_no 
                                    "changed  ---->        ?")
                              then Some body_orig
                              else None

                          | _ -> None
                        in
                        let body' =
                          match first_heuristic with
                          | Some x -> x
                          | None -> 
                              show_orig_view ~force_display:true
                                key s_orig s_view;


                              show_diff s_orig s_view;
                              pr2 "who is right ? orig ? view ? both ?  o/v/b ? ";
                              
                              let answer = read_line() in
                              (match answer with
                              | "o" -> body_orig
                              | "v" -> body_view
                              | "b" -> raise Todo
                              | _ -> failwith "not a valid answer"
                              )
                        in
                        ChunkDef({
                          chunkdef_key = key;
                          chunkdef_end = def.chunkdef_end;
                          chunkdef_id = def.chunkdef_id;
                        }, body')
                    )
                )
            )
        )
    )
  in

  (* check if have consumed every elements in the view *)
  h_view |> Common.hash_to_list |> List.iter (fun (k,v) -> 
    match !v with
    | [] -> ()
    | x::xs -> 
        pr2 ("Not consumed: " ^ k);
        pr2 ("<<<<<<<<<<<<<<<<");
        let strs = (x::xs) |> List.map snd |> List.map 
          Web_to_code.s_of_chunkdef_body in
        strs |> List.iter Common2.pr2_no_nl;
        pr2 (">>>>>>>>>>>>>>>>");
  );

  orig'
