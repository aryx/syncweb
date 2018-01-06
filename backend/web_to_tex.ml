(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

open Web

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(* Getting the tex code from a noweb file, 
 * a.k.a 'weaving' in Knuth's and literate programming terminology.
 * I could call this file weave.ml, but I always found this terminology
 * confusing.
 * 
 * pre-history:
 *  - 'noweblatexpad' hacky script supporting \t \n \l; preprocessor
 *    applied before noweave (of noweb)
 *  - decide to replace completely noweave of noweb (I already do not
 *    use notangle, so really now I use only noweb.sty), so easier to plugin
 *    code to support accurate defs/uses from codegraph
 * history:
 *  - basic tex output
 *  - support for left and right arrow and users of chunk
 *    (available in Icon version of totex plugin of noweb, found
 *     appropriate tex command by looking at source of totex and
 *     support.tex in github repo of noweb)
 *  - support for defs and uses
 *    (available in Icon version of noweb with the -autodefs options, found
 *     appropriate tex command by looking at output of 
 *    ~/packages/MacOS/stow/noweb/bin/noweave -autodefs c -index wc.nw
 *  - support for accurate defs and uses using codegraph information
 * 
 * less:
 *  - generate prev/next that works in two column; need to
 *    setup the glue thing?
 *  - generate chunk index?
 *  - Quote of code (recursive now), chunkname can also 
 *    contain some quote! need lexer functions? because recursive!
 *  - handle hreferenced_ref_already_recently via latex macro
 * later:
 *  - pretty printing? based again on pfff and codemap renderer?
 *    no color, but can still do stuff probably?
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
    (* pad's extension [<main()>] turns into a [[main()]]_subpageref!
     * Note that noweb allows to write [[<<function [[foo] ]>> ]] 
     * and you will get the subpageref, but this is annoying to type
     * and you get the 'function' extra prefix.
     *)
    | B of string (* [< >] *)

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
(* todo: ugly, should have file x loc information in Web.t *)
let last_chunkdef = ref ""
let error s =
  pr2 (spf "near:|%s|" !last_chunkdef);
  failwith s

(*****************************************************************************)
(* Parsing  *)
(*****************************************************************************)

let (parse_string: string -> tex_string) = fun s ->
  let xs = Common2.list_of_string s in
  let rec aux_string acc xs =
    match xs with
    | [] -> s_of_acc acc
    | '['::'<'::xs ->
      s_of_acc acc @ aux_brace [] xs
    | '['::'['::xs ->
      s_of_acc acc @ aux_quote [] xs
    | x::xs ->
      aux_string (x::acc) xs
  and aux_quote acc xs =
    match xs with
    | [] -> error "could not find end of ]]"
    | ']'::']'::xs ->
      Q (Common2.string_of_chars (List.rev acc))::
      aux_string [] xs
    | x::xs ->
      aux_quote (x::acc) xs
  and aux_brace acc xs =
    match xs with
    | [] -> error "could not find end of [<"
    | '>'::']'::xs ->
      B (Common2.string_of_chars (List.rev acc))::
      aux_string [] xs
    | x::xs ->
      aux_brace (x::acc) xs
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
    | '$' -> pr "{\\char36}"
    | '^' -> pr "{\\char94}"
    | '#' -> pr "{\\char35}"
    | '&' -> pr "{\\char38}"
    | '%' -> pr "{\\char37}"
    | '\\' -> pr "{\\char92}"
    | c -> pr (spf "%c" c)
  )

let pr_in_code pr s =
  let xs = Common2.list_of_string s in
  xs |> List.iter (fun c ->
    match c with     
    | '\\'-> pr "\\\\"
    | '{'-> pr "\\{"
    | '}'-> pr "\\}"
    | c -> pr (spf "%c" c)
  )

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let label_of_id = Crossref_chunk.label_of_id

(*****************************************************************************)
(* Entity crossrefs  *)
(*****************************************************************************)
let ident1_of_entity s =
  let xs = Common2.list_of_string s in
  xs |> List.map (fun c ->
    match c with
    | '_' -> "{\\_}"
    | c -> (spf "%c" c)
  ) |> String.concat ""

let ident2_of_entity s =
  let xs = Common2.list_of_string s in
  xs |> List.map (fun c ->
    match c with
    | '_' -> ":un"
    | c -> (spf "%c" c)
  ) |> String.concat ""

let nwixident_of_entity s suffix =
  let s1 = ident1_of_entity s in
  let s2 = ident2_of_entity s in
  spf "{\\nwixident{%s%s}}{%s}" s1 suffix s2
  

(* hack entity indexing based on pad's convention to name chunks *)
let pr_indexing pr hnwixident def =
  match def.chunkdef_key with
  (* todo: new format *)
  (* | s when s =~ "function \\[\\[\\(.*\\)()\\]\\]" -> *)
  | s when s =~ "\\(function\\|constructor\\|destructor\\|macro\\) \\[\\[\\(.*\\)\\]\\]" ->
    let _, f = Common.matched2 s in
    let nwident = nwixident_of_entity f "()" in
    Hashtbl.replace hnwixident nwident true;
    pr (spf "\\nwindexdefn%s{%s}" nwident (label_of_id def.chunkdef_id))
  | _ -> ()

let pr_final_index pr hnwixident = 
  hnwixident |> Common.hash_to_list |> List.map (fun (k, _bool) ->
    String.lowercase k, k) |> Common.sort_by_key_lowfirst 
  |> List.iter (fun (_k, k) ->
    pr (spf "\\nwixlogsorted{i}{%s}%%\n" k);
  )
      
(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

let web_to_tex orig texfile =
  Common.with_open_outfile texfile (fun (pr, _chan)  ->
  (* for nwbegincode{}, not sure it's needed *)
  let cnt = ref 0 in
  (* not sure it's needed *)
  let last = ref (spf "\\nwfilename{%s}" ("TODO.nw"))  in
  (* for referencing def of a chunkname *)
  let hkey_to_def = Crossref_chunk.hchunkname_to_def__from_orig orig in
  (* for prev/next def of chunkdefs *)
  let hchunkid_info = Crossref_chunk.hchunkid_info__from_orig orig in
  (* for remembering all the indexed entities when printing the final index *)
  let hnwixident = Hashtbl.create 101 in
  (* for [< >] and avoiding adding too many refs *)
  let hreferenced_def_already_recently = Hashtbl.create 101 in

  let rec tex_or_chunkdef x =
    match x with
    | Tex xs ->
      xs |> List.iter (fun s ->
        (match s with
        | _ when s =~ "#include +\"\\(.*\\.nw\\)\"" ->
          error "you must call Web.expand_sharp_include before"
        (* pad's special macros for todos and notes 
         * todo: get rid of noweblatex and code in main.ml for to_noweb? 
         *)
        | _ when s =~ "^\\\\[ntl] .*" -> 
    (* todo:
    $line =~ s/^[ \t]*\\t (.*  )$/\\SaveVerb{Verb}+$1+\\todo{\\UseVerb{Verb}}/;
    $line =~ s/^[ \t]*\\n (.*  )$/\\SaveVerb{Verb}+$1+\\note{\\UseVerb{Verb}}/;
    $line =~ s/^[ \t]*\\l (.*  )$/\\SaveVerb{Verb}+$1+\\less{\\UseVerb{Verb}}/;
    *)
          (* skip *)
          pr "%SKIPPED\n"
            
        (* hack, should use latex macro to have the same effect with
         * page granularity
         *)
        | "\\end{document}" -> 
          pr "\\nwenddocs{}\n";
          pr_final_index pr hnwixident;
          pr s

        | _ ->
          (match s with 
          | _ when s =~ "^\\\\chapter{" -> 
            Hashtbl.clear hreferenced_def_already_recently
          | _ when s =~ "^\\\\section{" -> 
            Hashtbl.clear hreferenced_def_already_recently
          | _ when s =~ "^\\\\subsection{" -> 
            Hashtbl.clear hreferenced_def_already_recently
          | _ when s =~ "^\\\\subsubsection{" -> 
            Hashtbl.clear hreferenced_def_already_recently
          | _ -> ()
          );
        let elts = parse_string s in
        elts |> List.iter (function
          | S s -> pr s
          | Q s ->
            pr "{\\tt{}";
            pr_in_quote pr s;
            pr "}";
          | B s ->
            (match s with
            (* special case {{foo()}} *)
            | _ when s =~ "^\\(.*\\)()$" ->
              let f = Common.matched1 s in
              (* todo: handle new format 'function [[foo()]]' *)
              let name_candidates = 
                [spf "function [[%s]]" f;
                 spf "constructor [[%s]]" f;
                 spf "destructor [[%s]]" f;
                 spf "macro [[%s]]" f;
                ]
              in
              let name = 
                try 
                  name_candidates |> List.find (fun x -> 
                    Hashtbl.mem hkey_to_def x)
                with Not_found -> 
                  error (spf "could not find def for |%s|" f)
              in
              let def = Hashtbl.find hkey_to_def name in
              pr "$\\texttt{";
              pr_in_quote pr s;
              pr "}";
              (if Hashtbl.mem hreferenced_def_already_recently name
              then ()
              else begin
                Hashtbl.add hreferenced_def_already_recently name true;
                pr (spf "^{\\subpageref{%s}}" 
                      (label_of_id def.chunkdef_id));
              end);
              pr "$";
              
            | _ -> error (spf "not handling yet {{}} format for: %s" s)
            )
           
        );
        pr "\n";
        )
      );
    | ChunkDef (def, ys) ->
      let chunk_info = Hashtbl.find hchunkid_info def.chunkdef_id in
      (* ugly hack *)
      last_chunkdef := def.chunkdef_key;
      incr cnt;
      pr !last;
      pr (spf "\\nwbegincode{%d}" !cnt);
      pr (spf "\\sublabel{%s}" (label_of_id def.chunkdef_id));

      pr "\\moddef{";
      let elts = parse_string def.chunkdef_key in
      elts |> List.iter (function
        | S s -> pr s
        | Q s -> 
          pr "\\code{}";
          pr s;
          pr "\\edoc{}";
        | B s ->
          error "{{ }} inside chunk definition is not allowed"
      );
      pr (spf "~{\\nwtagstyle{}\\subpageref{%s}}" (label_of_id def.chunkdef_id));
      (match chunk_info.prev_def with
      | None -> pr "}\\endmoddef"
      | Some _ -> pr "}\\plusendmoddef"
      );
      pr "\\nwstartdeflinemarkup";
      if chunk_info.chunk_users <> []
      then begin
        pr "\\nwusesondefline{";
        chunk_info.chunk_users |> List.iter (fun id ->
          pr (spf "\\\\{%s}" (label_of_id id))
        );
        pr "}";
      end;
      pr (spf "\\nwprevnextdefs{%s}{%s}"
            (match chunk_info.prev_def with
            | None -> "\\relax"
            | Some id -> label_of_id id
            )
            (match chunk_info.next_def with
            | None -> "\\relax"
            | Some id -> label_of_id id
            ));
      pr "\\nwenddeflinemarkup";
      pr "\n";

      (* recurse *)
      ys |> List.iter code_or_chunk;
      (* entity indexing *)
      pr_indexing pr hnwixident def;

      pr ("\\nwendcode{}");
      incr cnt;
      pr (spf "\\nwbegindocs{%d}\\nwdocspar" !cnt);
      last := "\\nwenddocs{}";
      pr "\n";
  and code_or_chunk x =
    match x with
    | Code s -> 
      pr_in_code pr s;
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
        | B s ->
          error "{{ }} inside a chunk name is not allowed"

      );
      let def = 
        try Hashtbl.find hkey_to_def s 
        with Not_found ->
          error (spf "Could not find def for |%s|" s)
          
      in
      pr (spf "~{\\nwtagstyle{}\\subpageref{%s}}" (label_of_id def.chunkdef_id));
      pr "\\RA{}";
      pr "\n";
  in
  List.iter tex_or_chunkdef orig;
  ()
  )
