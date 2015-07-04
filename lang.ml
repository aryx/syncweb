open Common
open Common2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
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

(*****************************************************************************)
(* Language specific handling in views  *)
(*****************************************************************************)


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
      | Some s -> raise Todo
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
      | Some s -> raise Todo
      )
    );
  }



(*****************************************************************************)
(* Final table  *)
(*****************************************************************************)

let lang_table = [
  "ocaml", mark_ocaml_short;
  "shell", mark_shell;
  "C",     mark_C_short;
  "ocamlyacc", mark_ocamlyacc_short;
  "php", mark_C_short;
  "haskell", mark_haskell_short;
]
