open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * history: started by defining types for orig and view that describes
 * how I want the two formats. Basically a list of stuff (aka chunks in
 * LP terminology) . Then wrote parse_orig, parse_view, the unparser,
 * and then view_of_orig and orig_of_view. Then finally wrote sync.
 * 
 * Later:
 *  - the md5sum marks can be in an auxillary file
 * 
 * note: took inspirations from nofake. Fun to see how Perl is quite
 * good. The nofake program is very short. But Lindig do multiple
 * things at the same time, parsing, building the hash, etc. 
 *)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* note: in fact it's not really a list but more an alternating list. We can
 * not have a Tex::Tex::_ normally. A Tex is always followed by a Chunkdef.
 * But is it worthwhile to be more precise ? hmmm in fact can have 
 * at least ChunkDef::ChunkDef::_ so probably not worthwhile.
 *)
type orig = tex_or_chunkdef list

 and tex_or_chunkdef =
   | Tex of string list (* why string list ? why not just string ? *)
   | ChunkDef of chunkdef * code_or_chunk list

   and chunkdef = {
    chunkdef_key: string;
    chunkdef_end: string; (* specific string *)
   }
   and code_or_chunk =
     | Code of string
     | ChunkName of string * int (* indentation *)


(* a view is a codetree list because a orig can contain multiple occurences
 * of the view name, that then must be appended to each other in the view
 *)
type view = codetree list

  and codetree = 
    | RegularCode of string
    | ChunkCode of 
        chunk_info * 
        codetree list (* have adjusted indentation *) *
        int (* indentation, local *)

     and chunk_info = { 
       chunk_key: string;

       (* advanced: md5sum corresponding code_or_chunk list in orig *)
       chunk_md5sum: string option; 

       mutable pretty_print: position option;
     }
     (* work with the less_marks flag *)
     and position = First | Middle | Last


(*****************************************************************************)
(* Language specific handling in views  *)
(*****************************************************************************)

type mark_language = {
  (* return (space, key, md5sum) *)
  parse_mark_start: string -> (string * string * string option) option;
  parse_mark_end: string -> (string * string option) option;

  unparse_mark_start: key:string -> md5:string option -> string;
  unparse_mark_end: key:string -> string;

  parse_mark_startend: string -> (string * string * string option) option;
  unparse_mark_startend: key:string -> md5:string option -> string;
}

let re_md5sum_in_aux_file = Str.regexp
  "\\(.*\\) |\\(.*\\)$"

(* todo: can even factorize as many use the same comment format.
 * Maybe can just even provide the tokens to make a comment ?
 * 
 * todo: withoyt key in endmark ? but need adjust parse_view
 *)

let mark_ocaml_short =
  {
    parse_mark_start = (fun s -> 
      if s =~ "\\([ \t]*\\)(\\*s: \\(.*\\) \\*)$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    parse_mark_end = (fun s -> 
      if s =~ "\\([ \t]*\\)(\\*e: \\(.*\\) \\*)$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      (match md5 with 
      | None -> spf "(*s: %s *)" key;
      | Some s -> raise Todo
      )
    );
    unparse_mark_end   = (fun ~key ->
      spf "(*e: %s *)" key
    );



    parse_mark_startend = (fun s -> 
      if s =~ "\\([ \t]*\\)(\\*x: \\(.*\\) \\*)$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    unparse_mark_startend = (fun ~key ~md5 ->
      (match md5 with
      | None -> spf "(*x: %s *)" key;
      | Some s -> raise Todo
      )
    );
  }

let mark_ocaml = 
  let re_start = Str.regexp 
  "\\([ \t]*\\)(\\* nw_s: \\(.*\\) |\\(.*\\)\\*)$"
  in
  let re_end = Str.regexp 
    "\\([ \t]*\\)(\\* nw_e: \\(.*\\) \\*)$"
  in
  {
 
   parse_mark_start = (fun s -> 
      if s ==~ re_start
      then 
        let (a,b,c) = Common.matched3 s in
        Some (a, b, Some c)
      else 
        None
    );

    parse_mark_end = (fun s -> 
      if s ==~ re_end
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );

    unparse_mark_start = (fun ~key ~md5 -> 
      spf "(* nw_s: %s |%s*)" key (match md5 with None -> "" | Some s -> s));
    unparse_mark_end   = (fun ~key ->      
      spf "(* nw_e: %s *)" key);

    parse_mark_startend = (fun s -> None);
    unparse_mark_startend = (fun ~key ~md5 -> raise Todo);
  }

let mark_shell = 
  let re_start = Str.regexp 
  "\\([ \t]*\\)# nw_s: \\(.*\\) |\\(.*\\)#$"
  in
  let re_end = Str.regexp 
    "\\([ \t]*\\)# nw_e: \\(.*\\) #$"
  in
  {
    parse_mark_start = (fun s -> 
      if s ==~ re_start
      then
        let (a,b,c) = Common.matched3 s in
        Some (a, b, if c = "" then None else Some c)
      else None
    );
    parse_mark_end = (fun s -> 
      if s ==~ re_end
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      spf "# nw_s: %s |%s#" key (match md5 with None -> "" | Some s -> s));
    unparse_mark_end   = (fun ~key ->      
      spf "# nw_e: %s #" key);

    parse_mark_startend = (fun s -> 
      None);
    unparse_mark_startend = (fun ~key ~md5 -> 
      raise Todo
    );
  }

let mark_ocamlyacc_short =
  {
    parse_mark_start = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*(\\*s: \\(.*\\) \\*)\\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    parse_mark_end = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*(\\*e: \\(.*\\) \\*)\\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      (match md5 with 
      | None -> spf "/*(*s: %s *)*/" key;
      | Some s -> raise Todo
      )
    );
    unparse_mark_end   = (fun ~key ->
      spf "/*(*e: %s *)*/" key
    );



    parse_mark_startend = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*(\\*x: \\(.*\\) \\*)\\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    unparse_mark_startend = (fun ~key ~md5 ->
      (match md5 with
      | None -> spf "/*(*x: %s *)*/" key;
      | Some s -> raise Todo
      )
    );
  }


let mark_C = 
  let re_start = Str.regexp 
  "\\([ \t]*\\)/\\* nw_s: \\(.*\\) |\\(.*\\)\\*/$"
  in
  let re_end = Str.regexp 
    "\\([ \t]*\\)/\\* nw_e: \\(.*\\) \\*/$"
  in
  {
    parse_mark_start = (fun s -> 
      if s ==~ re_start
      then
        let (a,b,c) = Common.matched3 s in
        Some (a, b, if c = "" then None else Some c)
      else None
    );
    parse_mark_end = (fun s -> 
      if s ==~ re_end
      then
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      spf "/* nw_s: %s |%s*/" key (match md5 with None -> "" | Some s -> s));
    unparse_mark_end   = (fun ~key ->      
      spf "/* nw_e: %s */" key);
  
    parse_mark_startend = (fun s -> None);
    unparse_mark_startend = (fun ~key ~md5 -> raise Todo);
  }

let mark_C_short =
  {
    parse_mark_start = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*s: \\(.*\\) \\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    parse_mark_end = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*e: \\(.*\\) \\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      (match md5 with 
      | None -> spf "/*s: %s */" key;
      | Some s -> raise Todo
      )
    );
    unparse_mark_end   = (fun ~key ->
      spf "/*e: %s */" key
    );



    parse_mark_startend = (fun s -> 
      if s =~ "\\([ \t]*\\)/\\*x: \\(.*\\) \\*/$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    unparse_mark_startend = (fun ~key ~md5 ->
      (match md5 with
      | None -> spf "/*x: %s */" key;
      | Some s -> raise Todo
      )
    );
  }





let lang_table = [
  "ocaml", mark_ocaml_short;
  "shell", mark_shell;
  "C",     mark_C_short;
  "ocamlyacc", mark_ocamlyacc_short;
  "php", mark_C_short;
]


let md5sum_auxfile_of_file file = 
  let (d,b) = Common.db_of_filename file in
  Common.filename_of_db (d, ".md5sum_" ^ b)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let generate_n_spaces i =
  Common.repeat " " i +> Common.join ""

let s_of_chunkdef_body xs = 
  xs +> List.map (function 
  | Code s -> s
  | ChunkName (s, i) -> 
      let spaces = generate_n_spaces i in
      spaces ^ (spf "<<%s>>" s)
  ) +> Common.unlines


let show_orig_view ?(force_display=false)key s_orig s_view = 
  pr2 ("DIFF for: " ^ key);
  if (Common.nblines s_orig > 5 || Common.nblines s_view > 5) && 
     not force_display 
  then 
    ()
  else begin
    pr2 "<<<<<<< orig <<<<<<<<";
    pr2_no_nl s_orig;
    pr2 "====================";
    pr2_no_nl s_view;
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
(* Parser  *)
(*****************************************************************************)

(* ------------------------------------------------------------ *)
(* orig *)
(* ------------------------------------------------------------ *)

(* First version, do as in nofake, very line-oriented and assume
 * a few things:
 *   - <<xx>>= and @ in first column and alone and on single line
 *   - <<yy>> inside chunk are singular, possibly with a prefix
 *     code and postfix code, but there is only one <<yy>> per line.
 * 
 * Have some limitations, but simpler, and in practice probably good enough.
 * 
 * todo: allow optional '@' when have a Start1 after another Start1.
 *)

type mark1 = Regular1 | Start1 | End1 

(* less: use pcre so can do .*? *)
let regexp_chunkdef = Str.regexp "^<<\\(.*\\)>>=[ \t]*$" 

let regexp_chunkdef_end = Str.regexp "^@[ \t]*$"

(* (.*[^\@]* )<<([^<>@]+)>>(. * ) *)
(* todo: more flexible ? *)
let regexp_chunk_ref = Str.regexp 
  "\\([ \t]*\\)<<\\(.*\\)>>[ \t]*$"

let key_and_index_chunk_ref_string s = 
  if s ==~ regexp_chunk_ref
  then
    let (space_or_tabs, key) = matched2 s in
    (* todo? handle tabs ? *)
    key, String.length space_or_tabs
  else failwith "not a chunk_ref string"

let key_of_chunckdef_string s = 
  if s ==~ regexp_chunkdef
  then matched1 s
  else failwith "not a chunkdef string"



let parse_orig file = 
  let xs = Common.cat file in 
  let xs' = xs +> Common.index_list_1 +> List.map (fun (s, i) -> 
    s, i,
    (match s with
    | _ when s ==~ regexp_chunkdef_end -> End1
    | _ when s ==~ regexp_chunkdef     -> Start1
    | _ -> Regular1
    )
  )
  in
  (* todo: more flexible *)
  let rec process_body ys =
    ys +> List.map (fun s -> 
      if s ==~ regexp_chunk_ref
      then
        let (key, indent) = key_and_index_chunk_ref_string s in
        ChunkName (key, indent)
      else Code s
    )
  in
  let rec agglomerate xs = 
    match xs with
    | [] -> []
    | x::xs -> 
        let line = snd3 x in 

        (match thd3 x with
        | Regular1 -> 
            let (regs, rest) = Common.span (fun x -> thd3 x = Regular1) xs in
            let item = Tex (fst3 x::(List.map fst3 regs)) in
            item::agglomerate rest
        | Start1 -> 
            (try 
              let (body, endmark, rest) = 
                Common.split_when (fun x -> thd3 x = End1) xs
              in
              if (not (body +> List.for_all (fun x -> thd3 x = Regular1)))
              then failwith 
                (spf "line %d: body of chunkdef contains other chunkdef" line);

              let body' = List.map fst3 body in

              let item = ChunkDef ({
                chunkdef_key = key_of_chunckdef_string (fst3 x);
                chunkdef_end = fst3 endmark;
              }, process_body body') in
              item::agglomerate rest
            with Not_found -> failwith "no end mark found"
            )

        | End1 -> failwith (spf "line %d: a end mark without a start" line)
        )
  in
  agglomerate xs'


                           

(* ------------------------------------------------------------ *)
(* view *)
(* ------------------------------------------------------------ *)
(* First version, again we assume line oriented and special cases.
 * Do special case for first chunck, generate chunk corresponding
 * to filename with fake prelude and postlude ?
 * 
 * Do multi files ? well no need I think, just call sync multiple 
 * times with the different view files.
 *)

(* for better error reporting *)
type pinfo = {
  file: filename;
  line: int;
}
let s_of_pinfo pinfo = 
  spf "%s:%d" pinfo.file pinfo.line
let mkp file line = 
  { file = file; line = line}

type mark2 = 
  | Regular2 of string * pinfo
  | Start2 of string * int * string option (* md5sum *) * pinfo
  | End2 of string option * int * pinfo

let readjust_mark2_remove_indent i body = 
  body +> List.map (function
  | Start2 (s, j, md5sum, pinfo) -> 
      if j < i 
      then failwith (s_of_pinfo pinfo ^ 
                     " nested chunk with smaller indentation at ");
      Start2 (s, j - i, md5sum, pinfo)
  | Regular2 (s, pinfo) -> 
      if Common.is_blank_string s 
      then Regular2 (s, pinfo)
      else 
        if s=~ "\\([ \t]*\\)\\(.*\\)"
        then
          let spaces, rest = matched2 s in
          let j = String.length spaces in 
          if j < i 
          then 
            failwith (s_of_pinfo pinfo ^ 
                     " nested chunk with smaller indentation at ");
          let spaces' = generate_n_spaces (j - i) in
          Regular2 (spaces' ^ rest, pinfo)
        else raise Impossible
  | End2 (x,i, pinfo) -> 
      (* dont care about End2 indent info *)
      End2 (x, i, pinfo) 
  )

(* patch the Start2 with the md5sums information in the md5sum_aux file *)
let readjust_start2_with_md5sums file xs = 
  if Sys.file_exists (md5sum_auxfile_of_file file)
  then 
    let md5s = 
      Common.cat (md5sum_auxfile_of_file file) +> 
        List.map (fun s -> 
          if s ==~ re_md5sum_in_aux_file
          then Common.matched2 s
          else failwith ("wrong format in md5sum_auxfile: " ^ s)
        )
    in
    let rec aux mark2s md5sums = 
      match mark2s, md5sums with
      | [], [] -> []
      | (Start2(s, j, md5sum, pinfo))::xs, (s2, md5sum2)::ys ->
          if s <> s2 
          then failwith 
            (spf "not same key in view and md5sum_auxfile: %s VS %s" s s2)
          else 
            if md5sum = None
            then
              (Start2(s, j, Some md5sum2, pinfo))::aux xs ys
            else
            failwith "md5sums present in view file"
      | ((End2 _|Regular2 _) as x)::xs, ys ->
          x::aux xs ys
      | (Start2(s, j, md5sum, pinfo))::xs, [] ->
          failwith
            "more marks in view file than md5sums in md5sum_auxfile"
      | [], y::ys ->
          failwith 
            "more md5sums in md5sum_auxfile than start marks in view file"
      
    in
    aux xs md5s

  else xs
  

(* old: was computing a first "implicit" chunk corresponding to the name if
 * the file, but not worth the extra complexity 
 *)
let parse_view ~lang file = 
  let xs = Common.cat file in 

  let xs' = xs +> Common.index_list_1 +> List.map (fun (s, line) -> 
    match lang.parse_mark_startend s with
    | Some (tabs, key,  md5) -> 
        [End2 (Some key, String.length tabs, mkp file line);
         Start2 (key, String.length tabs, md5, mkp file line);
        ]
    | None ->
        (match lang.parse_mark_start s with
        | Some (tabs, key,  md5) -> 
            [Start2 (key, String.length tabs, md5, mkp file line)]
        | None -> 
            (match lang.parse_mark_end s with
            | Some (tabs, key) -> 
                [End2 (key, String.length tabs, mkp file line)]
            | None -> 
                [Regular2 (s, mkp file line)]
            )
        )
  ) +> List.flatten +> readjust_start2_with_md5sums file
  in

  (* the view does not need to contain the key at the end mark; it's
   * redundant. But it is used for now to easily find the matching End2
   * of a Start2. If the key is not there, then have to find the
   * corresponding End2 by not stopping at the first one and by
   * counting.
   *)
  let rec aux xs = 
    match xs with
    | [] -> []
    | x::xs -> 
        (match x with
        | Start2 (s, i, md5sum, pinfo) -> 
            let (body, endmark, rest) = 
              Common.split_when (fun x -> match x with 
              | End2 (s2,_, pinfo2) -> 
                  (match s2 with
                  | None -> raise Todo
                  | Some s2 -> s = s2 
                  )
              | _ -> false
              ) xs
            in
            let body' = aux (readjust_mark2_remove_indent i body) in
            ChunkCode ({
              chunk_key = s;
              chunk_md5sum = md5sum;
              pretty_print = None;
            }, body', i)::aux rest
        | End2 (s, i, pinfo) -> 
            failwith (s_of_pinfo pinfo ^ " a end mark without a start at")
        | Regular2 (s, pinfo) -> 
            RegularCode s::aux xs
        )
  in
  let codetrees = aux xs' in
  codetrees

(*****************************************************************************)
(* Unparser *)
(*****************************************************************************)

let unparse_orig orig filename =
  Common.with_open_outfile filename (fun (pr_no_nl, _chan) -> 
    let pr s = pr_no_nl (s ^ "\n") in
    orig +> List.iter (function
    | Tex xs -> 
        xs +> List.iter pr;
    | ChunkDef (def, body) -> 
        let start = spf "<<%s>>=" def.chunkdef_key in
        let end_mark = def.chunkdef_end in
        pr start;
        body +> List.iter (function
        | Code s -> 
            pr s
        | ChunkName (s, indent) -> 
            Common.do_n indent (fun () -> pr_no_nl " ");
            let item = spf "<<%s>>" s in
            pr item;
        );
        pr end_mark;
    );
  )


let rec adjust_pretty_print_field view =
  match view with
  | [] -> ()
  | x::xs ->
      (match x with
      | RegularCode _ ->
          adjust_pretty_print_field xs
      | ChunkCode (info, _, indent) ->
          let same_key, rest = 
            Common.span (fun y -> 
              match y with
              | ChunkCode (info2, _, indent2) ->
                  info.chunk_key = info2.chunk_key &&
                  indent = indent2 (* always the same ? *)
              | _ -> false
            ) (x::xs)
          in
          (* recurse *)
          adjust_pretty_print_field rest;
          same_key +> List.iter (function
          | ChunkCode (_info, xs, i) ->
              adjust_pretty_print_field xs
          | _ -> raise Impossible
          );
          let same_key' = same_key +> List.map (function
            | ChunkCode(info, _, _) -> info
            | _ -> raise Impossible
          )
          in
          
          if List.length same_key' >= 2 then begin
            let (hd, middle, tl) = Common.head_middle_tail same_key' in
            hd.pretty_print <- Some First;
            tl.pretty_print <- Some Last;
            middle +> List.iter (fun x -> x.pretty_print <- Some Middle);
          end
      )
            


(* assume first chunkcode corresponds to the filename ? *)
let unparse_view 
  ?(md5sum_in_auxfile=false) 
  ?(less_marks=false)
  ~lang views filename 
 =
  let md5sums = ref [] in
  if less_marks 
  then adjust_pretty_print_field views;

  Common.with_open_outfile filename (fun (pr_no_nl, _chan) -> 
    let pr s = pr_no_nl (s ^ "\n") in
    let pr_indent indent = Common.do_n indent (fun () -> pr_no_nl " ") in

    let rec aux (x, body, i) =
      let key = x.chunk_key in
      let md5sum = 
        if md5sum_in_auxfile 
        then begin 
          Common.push2 (spf "%s |%s" key (Common.some x.chunk_md5sum))
            md5sums;
          None
        end
        else x.chunk_md5sum
      in

      pr_indent (i);
      (match x.pretty_print with
      | None | Some First ->
          pr (lang.unparse_mark_start key md5sum);
      | (Some (Middle|Last)) -> 
          pr (lang.unparse_mark_startend key md5sum);
      );
      body +> List.iter (function
      | RegularCode s -> 
          (* bugfix: otherwise make sync will not fixpoint *)
          if Common.is_blank_string s
          then pr s
          else begin
            pr_indent i;
            pr s;
          end
      | ChunkCode (x, body, j) -> 
          aux (x, body, i+j);

          (* if decide to not show toplevel chunk 
          let key = x.chunk_key in
          pr_indent (i+j);
          pr (spf "(* nw_s: %s |%s*)" key x.chunk_md5sum);
          aux (x, i+j);
          pr_indent (i+j);
          pr (spf "(* nw_e: %s *)" key);
          *)
      );
      (match x.pretty_print with
      | None | Some Last ->
          (* bugfix: the pr_indent call must be here, not outside *)
          pr_indent (i);
          pr (lang.unparse_mark_end key);
      | Some (First | Middle) ->
          ()
      )
    in

    views +> List.iter (function
    | ChunkCode (chunkcode, body, i) -> 
        aux (chunkcode, body, i)
    | RegularCode s -> 
        failwith "no chunk at toplevel"
    );
    ()
  );

  if md5sum_in_auxfile then begin
    Common.write_file ~file:(md5sum_auxfile_of_file filename)
      (!md5sums +> List.rev +> Common.join "\n");
  end;
  ()

(*****************************************************************************)
(* Invariants  *)
(*****************************************************************************)

(* let check_view v = 
 * 
 * can not have multiple chunkname with same key and different ident 
 * and not consecutive.
 * 
 *)

(* let check_orig x = 
*)

(*****************************************************************************)
(* Orig->View  *)
(*****************************************************************************)

let build_chunk_hash_from_orig orig = 
  let h = Hashtbl.create 101 in
    orig +> List.iter (function
    | Tex xs -> ()
    | ChunkDef (def, body) -> 
        let key = def.chunkdef_key in
        Common.hupdate_default key (fun x -> x @ [body]) (fun()->[]) h;
    );
  h


let view_of_orig ~topkey orig = 
  let h = build_chunk_hash_from_orig orig in

  let rec aux (key,i) = 
    let bodys = 
      try 
        Hashtbl.find h key 
      with Not_found -> 
        failwith (spf "view_of_orig: not able to find the chunkdef of '%s'" key)
    in

    bodys +> List.map (fun body -> 

      let s = s_of_chunkdef_body body in
      let md5sum = Common.md5sum_of_string s in

      let body' = 
        body +> List.map (function
        | Code s -> [RegularCode s]
        | ChunkName (s,i) -> 
            aux (s,i)
        ) +> List.flatten
      in

      ChunkCode ({
        chunk_key = key;
        chunk_md5sum = Some md5sum;
        pretty_print = None;
      }, body', i)
    )
  in
  aux (topkey, 0)

(*****************************************************************************)
(* View->Orig  *)
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
          body +> List.map (* and side effect *) (fun x -> 
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

        Common.hupdate_default key (fun x -> x @ [md5sum, body'']) 
          (fun()->[]) h;
        
    | RegularCode s -> 
        failwith ("code without enclosing chunk: " ^ s)
  in
  views +> List.iter aux;
  h
  


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
  let chunks = h +> Common.hash_to_list +> List.map (fun (k, v) -> k, ref v) in
  let h_view = Common.hash_of_list chunks in


  let h = build_chunk_hash_from_orig orig in
  let chunks = h +> Common.hash_to_list +> List.map (fun (k, v) -> k, ref v) in
  let h_orig = Common.hash_of_list chunks in

  (* we explore the orig in the original order *)
  let orig' = 
    orig +> List.map (function
    | Tex s -> 
        Tex s
    | ChunkDef (def, body_orig) -> 
        let key = def.chunkdef_key in
        
        (match Common.hfind_option key h_view with
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
                let s_orig = s_of_chunkdef_body body_orig in
                
                pr2 ("a chunk has been deleted or moved for: " ^ key);
                pr2 "<<<<<<< orig <<<<<<<<";
                pr2_no_nl s_orig;
                pr2 "====================";
                pr2 "keep the one in orig ? (y/n)";
                if (ask_y_or_no())
                then ChunkDef (def, body_orig)
                else failwith "stopped"
                
            | x::xs -> 

                (* no need try here *)
                let aref_orig = Hashtbl.find h_orig key in 
                
                (try 
                    (* Case3: equal chunk *)
                    let elem_view = 
                      !aref_view +> List.find (fun (md5, body_view) -> 
                        (* bugfix: have written body_view = body_view :) 
                         * type system can not catch such bugs :( 
                         *)
                        body_orig = body_view
                      )
                    in
                    aref_orig := Common.remove_first body_orig !aref_orig;
                    aref_view := Common.remove_first elem_view !aref_view;
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
                        aref_orig := Common.remove_first body_orig !aref_orig;
                        ChunkDef (def, body_orig)

                    | elem_view::_xs -> 
                        (* case4: multiple possible reasons.
                        *)

                        aref_orig := Common.remove_first body_orig !aref_orig;
                        aref_view := Common.remove_first elem_view !aref_view;
                    
                        let (md5sum_orig_in_view_opt, body_view) = elem_view in

                        let md5sum_past = 
                          match md5sum_orig_in_view_opt with
                          | None -> 
                              failwith "TODO: didnt find the md5sum"
                          | Some s -> s
                        in

                        let s_orig = s_of_chunkdef_body body_orig in
                        let s_view = s_of_chunkdef_body body_view in

                        let md5sum_orig = Common.md5sum_of_string s_orig in
                        let md5sum_view = Common.md5sum_of_string s_view in

                        show_orig_view key s_orig s_view;

                        pr2 "orig          view";
                        (* ask choice or merge *)

                        let first_heuristic = 
                          match () with
                          | _ when md5sum_past =$= md5sum_orig -> 
                              show_diff s_orig s_view;
                              pr2 "        <---- changed ? (y/n) ";
                              if (ask_y_or_no())
                              then Some body_view
                              else None
                                
                          | _ when md5sum_past =$= md5sum_view -> 
                              show_diff s_view s_orig;
                              pr2 "changed  ---->        ? (y/n) ";
                              if (ask_y_or_no())
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
                        }, body')
                    )
                )
            )
        )
    )
  in

  (* check if have consumed every elements in the view *)
  h_view +> Common.hash_to_list +> List.iter (fun (k,v) -> 
    match !v with
    | [] -> ()
    | x::xs -> 
        pr2 ("Not consumed: " ^ k);
        pr2 ("<<<<<<<<<<<<<<<<");
        let strs = (x::xs) +> List.map snd +> List.map s_of_chunkdef_body in
        strs +> List.iter pr2_no_nl;
        pr2 (">>>>>>>>>>>>>>>>");
  );

  orig'

(* ------------------------------------------------------------ *)
(* special case of generation *)
(* ------------------------------------------------------------ *)

(*****************************************************************************)
(* Multi file support  *)
(*****************************************************************************)
(* For the moment the multi file support is really a hack. I just 
 * abuse the Tex constructor to remember that a serie of tex_or_chunkdef 
 * belongs to a file. This hack allows to use 'sync' as-is, without any
 * additional coding, for free.
 * 
 * Assumption: the file list are given in a good order, the order of 
 * the #include inside the .nw files. Moreover it assumes the appended chunks
 * are defined in the good order too. As most of the time I use the multi
 * file in a very basic way, just to split a big .nw. this is not a problem
 * I think.
 * 
 * less? could introduce a MultiFileHack of Common.filename constructor
 * instead of abusing Tex
 *)

let pack_multi_orig xs = 
  xs +> List.map (fun (file, xs) -> 
    Tex (["MULTIFILE:" ^ file])::xs
  ) +> List.flatten


let rec unpack_multi_orig orig =
  let (pre, groups) = 
    Common.group_by_pre (fun x ->
      match x with
      | Tex [s] when s =~ "MULTIFILE:.*" -> true
      | _ -> false
    ) orig
  in
  if not (null pre)
  then failwith "could not find a MULTIFILE mark in packed orig, weird";

  groups +> List.map (fun (x, xs) ->
    match x with
    | Tex [s] when s =~ "MULTIFILE:\\(.*\\)$" ->
        Common.matched1 s, xs
    | _ -> raise Impossible
  )

(*****************************************************************************)
(* Tests  *)
(*****************************************************************************)

(*****************************************************************************)
(* Actions  *)
(*****************************************************************************)

let actions () = [
  "-parse_orig", "   <file>", 
    Common.mk_action_1_arg (fun x -> 
      let tmpfile = "/tmp/xxx" in
      let orig = parse_orig x in
      unparse_orig orig tmpfile;
      Common.command2(spf "diff %s %s" x tmpfile);
    );
  "-parse_view", "   <file>", 
    Common.mk_action_1_arg (fun x -> 
      ignore(parse_view ~lang:mark_ocaml_short x);
    );

  "-view_of_orig", "   <file> <key>", 
    Common.mk_action_2_arg (fun x key -> 
      let orig = parse_orig x in
      let view = view_of_orig key orig in
      let tmpfile = "/tmp/xxx" in
      unparse_view ~lang:mark_ocaml view tmpfile;
      tmpfile +> Common.cat +> List.iter pr;
      (*Common.command2(spf "diff %s %s" x tmpfile); *)
    );

  (* superseded by Main.main_action now *)
  "-sync", "   <orig> <view>", 
    Common.mk_action_2_arg (fun origf viewf -> 
      let orig = parse_orig origf in
      let views = parse_view ~lang:mark_ocaml viewf in

      let orig' = sync ~lang:mark_ocaml     orig views  in

      let tmpfile = "/tmp/xxx" in
      unparse_orig orig' tmpfile;
      Common.command2(spf "diff %s %s" origf tmpfile);
    );
  "-unmark", "   <file>", 
    Common.mk_action_1_arg (fun file -> 

      let xs = Common.cat file in
      let xs = xs +> Common.exclude (fun s ->
        s =~ "^[ \t]*(\\*[sex]:"
      )
      in
      let tmpfile = "/tmp/xxx" in
      let s = Common.unlines xs in
      Common.write_file tmpfile s;
      Common.command2(spf "diff -u %s %s" file tmpfile);
      pr2 "apply modif [y/n]?";
      if Common.ask_y_or_no ()
      then Common.write_file file s
      else failwith "ok, skipping"
    );
]
