(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

open Web

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * a.k.a 'weave' in Knuth's and literate programming terminology.
 * I could call this file weave.ml, but I always found this terminology
 * confusing.
 *)

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)
type tex_string = elt list
  and elt = 
    | S of string
    (* potentially you could have a chunkname inside a Quote, so
     * it should be Quote of code_or_chunk list 
     *)
    | Q of string (* [[ ]] *)

(*****************************************************************************)
(* Parsing  *)
(*****************************************************************************)

let (parse_string: string -> tex_string) = fun s ->
  let xs = Common2.list_of_string s in
  let rec aux_string acc xs =
    match xs with
    | [] -> s_of_acc acc
    | '['::'['::xs ->
      s_of_acc acc @ aux_quote [] xs
    | x::xs ->
      aux_string (x::acc) xs
  and aux_quote acc xs =
    match xs with
    | [] -> failwith "could not find end of quote"
    | ']'::']'::xs ->
      Q (Common2.string_of_chars (List.rev acc))::
      aux_string [] xs
    | x::xs ->
      aux_quote (x::acc) xs
  and s_of_acc acc =
    if acc = []
    then []
    else [S (Common2.string_of_chars (List.rev acc))]
  in
  aux_string [] xs

(*****************************************************************************)
(* Unparsing  *)
(*****************************************************************************)

let generate_n_spaces i =
  Common2.repeat " " i |> Common.join ""

let pr_in_quote pr s =
  let xs = Common2.list_of_string s in
  xs |> List.iter (fun c ->
    match c with
    | '_' -> pr "{\\char95}"
    | c -> pr (spf "%c" c)
  )

(*****************************************************************************)
(* Codegraph automatic indexing  *)
(*****************************************************************************)

(*****************************************************************************)
(* Chunk crossrefs  *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

let web_to_tex orig texfile =
  Common.with_open_outfile texfile (fun (pr, _chan)  ->
    (* pad's special macros for todos and notes 
     * todo: get rid of noweblatex and code in main.ml for to_noweb? 
     *)
  let cnt = ref 0 in
  let hdefs = Hashtbl.create 101 in

  let rec tex_or_chunkdef x =
    match x with
    | Tex xs ->
      xs |> List.iter (fun s ->
        let elts = parse_string s in
        elts |> List.iter (function
          | S s -> pr s
          | Q s ->
            pr "{\\tt{}";
            pr_in_quote pr s;
            pr "}";
        );
        pr "\n";
      );
    | ChunkDef (def, ys) ->
      incr cnt;
      pr (spf "\\nwbegincode{%d}" !cnt);

      pr "\\moddef{";
      let elts = parse_string def.chunkdef_key in
      elts |> List.iter (function
        | S s -> pr s
        | Q s -> 
          pr "\\code{}";
          pr s;
          pr "\\edoc{}";
      );
      (if Hashtbl.mem hdefs def.chunkdef_key
      then pr "}\\plusendmoddef"
      else pr "}\\endmoddef"
      );
      Hashtbl.replace hdefs def.chunkdef_key true;
      pr "\n";

      ys |> List.iter code_or_chunk;

      pr ("\\nwendcode{}\n");
  and code_or_chunk x =
    match x with
    | Code s -> 
      pr s;
      pr "\n";
    | ChunkName (s, i) ->
      pr (generate_n_spaces i);
      pr "\\LA{}";
      let elts = parse_string s in
      elts |> List.iter (function
        | S s -> pr s
        | Q s -> 
          pr "\\code{}";
          pr s;
          pr "\\edoc{}";
      );
      pr "\\RA{}";
      pr "\n";
  in
  List.iter tex_or_chunkdef orig;
  ()
  )
