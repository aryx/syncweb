(* Copyright 2009-2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* a view is a codetree list because a orig can contain multiple occurences
 * of the view name, that then must be appended to each other in the view
 *)
type t = codetree list

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
     (* work with the -less_marks flag *)
     and position = First (* s: *) | Middle (* x: *) | Last (* e: *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
let generate_n_spaces i =
  Common2.repeat " " i |> Common.join ""

let (==~) s re =
    Str.string_match re s 0

(*****************************************************************************)
(* md5sum  *)
(*****************************************************************************)

let md5sum_auxfile_of_file file = 
  let (d,b) = Common2.db_of_filename file in
  let oldformat = Common2.filename_of_db (d, ".md5sum_" ^ b) in
  if Sys.file_exists oldformat
  then oldformat
  else 
    let (d,b,e) = Common2.dbe_of_filename file in
    (* works better with codemap, and also mkmany in plan9 *)
    Common2.filename_of_dbe (d, spf ".md5sum_%s_%s" b e, "")

let re_md5sum_in_aux_file = Str.regexp
  "\\(.*\\) |\\(.*\\)$"

(*****************************************************************************)
(* Parser  *)
(*****************************************************************************)

(* First version, again we assume line oriented and special cases.
 * Do special case for first chunk, generate chunk corresponding
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
  body |> List.map (function
  | Start2 (s, j, md5sum, pinfo) -> 
      if j < i 
      then failwith (s_of_pinfo pinfo ^ 
                     " nested chunk with smaller indentation at ");
      Start2 (s, j - i, md5sum, pinfo)
  | Regular2 (s, pinfo) -> 
      if Common2.is_blank_string s 
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
      Common.cat (md5sum_auxfile_of_file file) |> 
        List.map (fun s -> 
          if s ==~ re_md5sum_in_aux_file
          then Common.matched2 s
          else failwith ("wrong format in md5sum_auxfile: " ^ s)
        )
    in
    let rec aux mark2s md5sums = 
      match mark2s, md5sums with
      | [], [] -> []
      | (Start2(s, j, md5sum, pinfo) as x)::xs, (s2, md5sum2)::ys ->
          if s <> s2 
          then begin 
            pr2 (spf "not same key in view and md5sum_auxfile: %s VS %s" s s2);
            if (Common2.y_or_no 
                  "This may be because you moved entities. Continue?")
            then x::xs
            else failwith "Stop here"
            
          end
          else 
            if md5sum = None
            then
              (Start2(s, j, Some md5sum2, pinfo))::aux xs ys
            else
            failwith "md5sums present in view file"
      | ((End2 _|Regular2 _) as x)::xs, ys ->
          x::aux xs ys
      | (Start2(s, j, md5sum, pinfo) as x)::xs, [] ->
          pr2 "more marks in view file than md5sums in md5sum_auxfile";
          if (Common2.y_or_no 
                "This may be because you moved entities. Continue?")
          then x::xs
          else failwith "Stop here"
          
      | [], y::ys ->
          pr2 "more md5sums in md5sum_auxfile than start marks in view file";
          if (Common2.y_or_no 
                "This may be because you moved entities. Continue?")
          then []
          else failwith "Stop here"
      
    in
    aux xs md5s

  else xs
  

(* old: was computing a first "implicit" chunk corresponding to the name if
 * the file, but not worth the extra complexity 
 *)
let parse ~lang file = 
  let xs = Common.cat file in 

  let xs' = xs |> Common.index_list_1 |> List.map (fun (s, line) -> 
    match lang.Lang.parse_mark_startend s with
    | Some (tabs, key,  md5) -> 
        [End2 (Some key, String.length tabs, mkp file line);
         Start2 (key, String.length tabs, md5, mkp file line);
        ]
    | None ->
        (match lang.Lang.parse_mark_start s with
        | Some (tabs, key,  md5) -> 
            [Start2 (key, String.length tabs, md5, mkp file line)]
        | None -> 
            (match lang.Lang.parse_mark_end s with
            | Some (tabs, key) -> 
                [End2 (key, String.length tabs, mkp file line)]
            | None -> 
                [Regular2 (s, mkp file line)]
            )
        )
  ) |> List.flatten |> readjust_start2_with_md5sums file
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
             try 
              Common2.split_when (fun x -> match x with 
              | End2 (s2,_, pinfo2) -> 
                  (match s2 with
                  | None -> raise Todo
                  | Some s2 -> s = s2 
                  )
              | _ -> false
              ) xs
             with Not_found ->
               failwith (s_of_pinfo pinfo ^ " could not find end mark")
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
          same_key |> List.iter (function
          | ChunkCode (_info, xs, i) ->
              adjust_pretty_print_field xs
          | _ -> raise Impossible
          );
          let same_key' = same_key |> List.map (function
            | ChunkCode(info, _, _) -> info
            | _ -> raise Impossible
          )
          in
          
          if List.length same_key' >= 2 then begin
            let (hd, middle, tl) = Common2.head_middle_tail same_key' in
            hd.pretty_print <- Some First;
            tl.pretty_print <- Some Last;
            middle |> List.iter (fun x -> x.pretty_print <- Some Middle);
          end
      )
            


(* assume first chunkcode corresponds to the filename ? *)
let unparse 
  ?(md5sum_in_auxfile=false) 
  ?(less_marks=false)
  ~lang views filename 
 =
  let md5sums = ref [] in
  if less_marks 
  then adjust_pretty_print_field views;

  Common.with_open_outfile filename (fun (pr_no_nl, _chan) -> 
    let pr s = pr_no_nl (s ^ "\n") in
    let pr_indent indent = Common2.do_n indent (fun () -> pr_no_nl " ") in

    let rec aux (x, body, i) =
      let key = x.chunk_key in
      let md5sum = 
        if md5sum_in_auxfile 
        then begin 
          Common.push (spf "%s |%s" key (Common2.some x.chunk_md5sum))
            md5sums;
          None
        end
        else x.chunk_md5sum
      in

      pr_indent (i);
      (match x.pretty_print with
      | None | Some First ->
          pr (lang.Lang.unparse_mark_start key md5sum);
      | (Some (Middle|Last)) -> 
          pr (lang.Lang.unparse_mark_startend key md5sum);
      );
      body |> List.iter (function
      | RegularCode s -> 
          (* bugfix: otherwise make sync will not fixpoint *)
          if Common2.is_blank_string s
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
          pr (lang.Lang.unparse_mark_end key);
      | Some (First | Middle) ->
          ()
      )
    in

    views |> List.iter (function
    | ChunkCode (chunkcode, body, i) -> 
        aux (chunkcode, body, i)
    | RegularCode s -> 
        failwith "no chunk at toplevel"
    );
    ()
  );

  if md5sum_in_auxfile then begin
    Common.write_file ~file:(md5sum_auxfile_of_file filename)
      (!md5sums |> List.rev |> Common.join "\n");
  end;
  ()
