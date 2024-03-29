(* Copyright 2009-2016 Yoann Padioleau, see copyright.txt *)
open Common

module S = Signature
 
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mark_language = {
  (* 's:' return (space, key, signature option) *)
  parse_mark_start: string -> (string * string * Signature.t option) option;
  (* 'e:' return (space, key option) *)
  parse_mark_end: string -> (string * string option) option;

  unparse_mark_start: key:string -> md5:Signature.t option -> string;
  unparse_mark_end: key:string -> string;

  (* 'x:' works with less_marks flag *)
  parse_mark_startend: string -> (string * string * Signature.t option) option;
  unparse_mark_startend: key:string -> md5:Signature.t option -> string;
}

(* from common2.ml *)
let (==~) s re =
  Str.string_match re s 0

(*****************************************************************************)
(* Language specific handling in views  *)
(*****************************************************************************)

(* todo: could factorize because many languages use the same comment format.
 * Maybe we could just provide the tokens to make a comment ?
 * todo: without key in endmark ? but need adjust parse_view
 *)

(* no mark in the comment, shorter format because of md5sum_in_auxfile *)
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
      | Some _ -> failwith "this language works only with -md5sum_in_auxfile"
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
      | Some _ -> failwith "this language works only with -md5sum_in_auxfile"
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
        Some (a, b, Some (S.from_hex c))
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
      spf "(* nw_s: %s |%s*)" key 
        (match md5 with None -> "" | Some s -> S.to_hex s));
    unparse_mark_end   = (fun ~key ->      
      spf "(* nw_e: %s *)" key);

    parse_mark_startend = (fun _ -> None);
    unparse_mark_startend = (fun ~key ~md5 ->
      ignore(key, md5);
      failwith "-less_marks is not supported for this language"
    );
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
        Some (a, b, if c = "" then None else Some (S.from_hex c))
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
      spf "# nw_s: %s |%s#" key 
        (match md5 with None -> "" | Some s -> S.to_hex s));
    unparse_mark_end   = (fun ~key ->      
      spf "# nw_e: %s #" key);

    parse_mark_startend = (fun _ -> 
      None);
    unparse_mark_startend = (fun ~key ~md5 -> 
      ignore(key, md5);
      failwith "-less_marks is not supported for this language"
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
      | Some _ -> failwith "this language works only with -md5sum_in_auxfile"
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
      | Some _ -> failwith "this language works only with -md5sum_in_auxfile"
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
        Some (a, b, if c = "" then None else Some (S.from_hex c))
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
      spf "/* nw_s: %s |%s*/" key 
        (match md5 with None -> "" | Some s -> S.to_hex s));
    unparse_mark_end   = (fun ~key ->      
      spf "/* nw_e: %s */" key);
  
    parse_mark_startend = (fun _ -> None);
    unparse_mark_startend = (fun ~key ~md5 -> 
      ignore(key, md5);
      failwith "-less_marks is not supported for this language"
    );
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
      | Some _s -> failwith "this language works only with -md5sum_in_auxfile"
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
      | Some _s -> failwith "this language works only with -md5sum_in_auxfile"
      )
    );
  }

let mark_haskell_short =
  {
    parse_mark_start = (fun s -> 
      if s =~ "\\([ \t]*\\){-s: \\(.*\\) -}$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    parse_mark_end = (fun s -> 
      if s =~ "\\([ \t]*\\){-e: \\(.*\\) -}$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, Some b)
      else None
    );
    unparse_mark_start = (fun ~key ~md5 -> 
      (match md5 with 
      | None -> spf "{-s: %s -}" key;
      | Some _s -> failwith "this language works only with -md5sum_in_auxfile"
      )
    );
    unparse_mark_end   = (fun ~key ->
      spf "{-e: %s -}" key
    );



    parse_mark_startend = (fun s -> 
      if s =~ "\\([ \t]*\\){-x: \\(.*\\) -}$"
      then 
        let (a,b) = Common.matched2 s in
        Some (a, b, None)
      else 
        None
    );
    unparse_mark_startend = (fun ~key ~md5 ->
      (match md5 with
      | None -> spf "{-x: %s -}" key;
      | Some _s -> failwith "this language works only with -md5sum_in_auxfile"
      )
    );
  }

(*****************************************************************************)
(* Final table  *)
(*****************************************************************************)

let lang_table auxfile = [
  "ocaml", if auxfile then mark_ocaml_short else mark_ocaml;
  "C",     if auxfile then mark_C_short else mark_C;
  "shell", mark_shell;
  "ocamlyacc", mark_ocamlyacc_short;
  "php", mark_C_short;
  "haskell", mark_haskell_short;
]
