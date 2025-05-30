(* Copyright 2009-2025 Yoann Padioleau, see copyright.txt *)

open Common
open Fpath_.Operators
module E = Entity_code
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core algorithms behind the lpizer except the cmdline parsing
 * (done in Main.ml)
*)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* TODO: move to Main.ml? make lang (and use Lang.t?) a parameter of lpize! 
 * instead of all those hardcoded CONFIG in this file.
*)
(* CONFIG *)
let lang = ref "ml"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* In this file because it can't be put in syncweb/ (it uses graph_code_c),
 * and it's a form of code slicing ...
 *)

(* syncweb does not like tabs *)
let untabify s =
  Str.global_substitute (Str.regexp "^\\([\t]+\\)") (fun _wholestr ->
    let substr = Str.matched_string s in
    let n = String.length substr in
    Common2_.n_space (4 * n)
  ) s

(* todo: could generalize this in graph_code.ml! have a range
 * property there!
 *)
type entity = {
  name: string;
  kind: Entity_code.kind;
  range: int * int;
}

type env = {
  (* TODO: Fpath.t *)
  current_file: string;
  cnt: int ref;
  hentities: (Graph_code.node, bool) Hashtbl.t;
}

let uniquify env kind s =
  let sfinal =
    if Hashtbl.mem env.hentities (s, kind) 
    then
      let s2 = spf "%s (%s)" s env.current_file in
      if Hashtbl.mem env.hentities (s2, kind)
      then begin
        incr env.cnt;
        let s3 = spf "%s (%s)%d" s env.current_file !(env.cnt) in
        if Hashtbl.mem env.hentities (s3, kind)
        then failwith "impossible"
        else s3
      end
      else s2
    else s
  in
  Hashtbl.replace env.hentities (sfinal, kind) true;
  sfinal

let count_dollar s =
  let cnt = ref 0 in
  for i = 0 to String.length s - 1 do
    if String.get s i =$= '$'
    then incr cnt
  done;
  !cnt

(*****************************************************************************)
(* Lang specific *)
(*****************************************************************************)

(*--------------------------------------------------*)
(* C *)
(*--------------------------------------------------*)

(*
module Ast = Ast_cpp
open Ast_cpp
module E = Entity_code

(*
let hooks_for_comment_cpp = { Comment_code.
    kind = Token_helpers_cpp.token_kind_of_tok;
    tokf = Token_helpers_cpp.info_of_tok;
    }

let range_of_any_with_comment_cpp any toks =
  let ii = Lib_parsing_cpp.ii_of_any any in
  let (min, max) = PI.min_max_ii_by_pos ii in
  match Comment_code.comment_before hooks_for_comment_cpp min toks with
  | None -> min, max
  | Some ii -> ii, max
*)
 

(* todo: 
%  - could do that in graph_code_c, with a range 
 * - if have functions like this
 *     asize_t fl_cur_size = 0;         /* How many free words were added since
 *                                      the latest fl_init_merge. */
 *    then the comment is splitted in two which leads to parse error
 *    in the generated file
 * - do not create chunk entity for #define XXx when just after a ifndef XXx
 *   at the top of the file.
 *)
let extract_entities_cpp env xs =
  xs |> List.filter_map (fun (top, toks) ->
    match top with
    | CppDirectiveDecl decl ->
      (match decl with
      | Define (_, ident, kind_define, _val) ->
        let kind =
          match kind_define with
            | DefineVar -> E.Constant
            | DefineFunc _ -> E.Macro
        in
        let (min, max) = range_of_any_with_comment_cpp (Toplevel top) toks in
        Some {
          name = fst ident |> uniquify env kind;
          kind = kind;
          range = (PI.line_of_info min, PI.line_of_info max);
        }
      | _ -> None
      )

    | DeclElem decl ->
      (match decl with
      | Func (FunctionOrMethod def) ->
        let (min, max) = range_of_any_with_comment_cpp (Toplevel top) toks in
        Some { 
          name = Ast.string_of_name_tmp def.f_name |> uniquify env E.Function;
          kind = E.Function;
          range = (PI.line_of_info min, PI.line_of_info max);
        }
      | BlockDecl decl ->
        let (min, max) = range_of_any_with_comment_cpp (Toplevel top) toks in
          (match decl with
          | DeclList ([x, _], _) ->
              (match x with
              (* prototype, don't care *)
              | { v_namei = Some (_name, None);
                  v_type = (_, (FunctionType _)); _
                } -> None

              (* typedef struct X { } X *)
              | { v_namei = _;
                  v_type = (_, (StructDef { c_name = Some name; _})); 
                  v_storage = StoTypedef _; _
                } -> 
                Some { 
                  name = Ast.string_of_name_tmp name |> uniquify env E.Class;
                  kind = E.Class;
                  range = (PI.line_of_info min, PI.line_of_info max);
                }

              (* other typedefs, don't care *)
              | { v_namei = Some (_name, None);
                  v_storage = StoTypedef _; _
                } -> None
              (* global decl, don't care *)
              | { v_namei = Some (_name, None);
                  v_storage = (Sto (Extern, _)); _
                } -> None


              (* global def *)
              | { v_namei = Some (name, _);
                  v_storage = _; _
                } -> 
                Some { 
                  name = Ast.string_of_name_tmp name |> uniquify env E.Global;
                  kind = E.Global;
                  range = (PI.line_of_info min, PI.line_of_info max);
                }
              (* struct def *)
              | { v_namei = _;
                  v_type = (_, (StructDef { c_name = Some name; _})); _
                } -> 
                Some { 
                  name = Ast.string_of_name_tmp name |> uniquify env E.Class;
                  kind = E.Class;
                  range = (PI.line_of_info min, PI.line_of_info max);
                }
              (* enum def *)
              | { v_namei = _;
                  v_type = (_, (EnumDef (_, Some ident, _)));
                  _
                } -> 
                Some { 
                  name = 
                    Ast.string_of_name_tmp (None, [], IdIdent ident) 
                      |> uniquify env E.Type;
                  kind = E.Type;
                  range = (PI.line_of_info min, PI.line_of_info max);
                }

              (* enum anon *)
              | { v_namei = _;
                  v_type = (_, (EnumDef (_, None, _)));
                  _
                } -> 
                Some { 
                  name = "_anon_"  |> uniquify env E.Type;
                  kind = E.Type;
                  range = (PI.line_of_info min, PI.line_of_info max);
                }
                

              | _ -> None
              )
          | _ -> None
          )
      | _ -> None
      )
    | _ -> None
  )

*)
(*--------------------------------------------------*)
(* OCaml *)
(*--------------------------------------------------*)

let hooks_for_comment_ml = { Comment_code.
    kind = Token_helpers_ml.token_kind_of_tok;
    tokf = Token_helpers_ml.info_of_tok;
    }

let is_estet_comment (ii : Tok.t) =
  let s = Tok.content_of_tok ii in
  (s =~ ".*\\*\\*\\*\\*") ||
  (s =~ ".*------") ||
  (s =~ ".*####")

let range_of_any_with_comment_ml (any : AST_ocaml.any) (toks : Parser_ml.token list) : Tok.t * Tok.t =
  let hooks = hooks_for_comment_ml in
  let any_gen = Ocaml_to_generic.any any in
  let ii = 
    AST_generic_helpers.ii_of_any any_gen |> List.filter Tok.is_origintok 
  in
  let (min, max) = 
    Tok_range.min_max_toks_by_pos ii 
  in

  let if_not_estet_comment ii =
    if not (is_estet_comment ii)
    then Some ii else None
  in
  let comment_before_opt =
    let* i1 = Comment_code.comment_before hooks min toks in
    if_not_estet_comment i1
  in
  let _comment_after_opt = 
    Comment_code.comment_after hooks max toks
  in
  let toks_after =
    let after = Comment_code.toks_after hooks max toks in
    Comment_code.drop_space_and_newline hooks after
  in
  let min = comment_before_opt ||| min in
  let after_opt =
    match toks_after with
    | x :: _xs  ->
        (match hooks.kind x with
        | PI.RPar | PI.RBrace | PI.RBracket | RAngle -> 
              Some (hooks.tokf x)
        | _ -> None
        )
    | _ -> None
  in
  let max = after_opt ||| max in
  min, max

open AST_ocaml

let nb_newlines (info : Tok.t) =
  let str = Tok.content_of_tok info in
  if str =~ ".*\n"
  then (Str.split_delim (Str.regexp "\n") str |> List.length) - 1
  else 0

(* TODO: 
 * - let f = function ... leads to constant!! should be function! 
 *)
let extract_entities_ml (env : env) (ast : program) (toks : Parser_ml.token list) : entity list =
  let qualify x = 
    Module_ml.module_name_of_filename env.current_file ^ "." ^ x 
  in
  let range any = 
    let (min, max) = range_of_any_with_comment_ml any toks in
    let nblines = nb_newlines max in
    (Tok.line_of_tok min, Tok.line_of_tok max + nblines)
  in

  let cnt = ref 0 in
  ast |> List.map (fun top ->
      let mk_entity kind name = 
        {
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }
      in
      match top.i with
      (* the different kinds of let *)
      | Let(_i1, _, [LetClassic(
                              {lname= (name, _); 
                               lparams=[];
                               lbody=Function(_, _);
                               _
                              })]) ->
        [mk_entity E.Function name]

      | Let(_i1, _,[LetClassic({lname=(name, _); lparams=[]; _})]) ->
        [mk_entity E.Constant name]

      (* TODO: what about let rec ... and ... *)
      | Let(_i1, _, [LetClassic(
                              {lname=(name, _); lparams=_::_; _})]) ->
        [mk_entity E.Function name]

      | Let(_i1, _, [LetPattern(PatUnderscore _, _)]) ->
        incr cnt;
        [mk_entity E.TopStmts (spf "_%d" !cnt)]

      | Exception(_, (name, _), _) ->
        [mk_entity E.Exception name]

      | Val (_, (name, _), _) ->
        [mk_entity E.Prototype name]

      | Type(_, [TyDecl({tname = (name, _); _})]) ->
        [mk_entity E.Type name]

      (* type x = ... and y = ... *)
      | Type(ttype, xs) ->
          let idx = ref 0 in
          xs |> List.filter_map (fun x ->
              incr idx;
              match x with
              | TyDecl({tname = (name, _);_}) -> 
                  let kind = E.Type in
                  let any : any = 
                    if !idx =|= 1
                    then 
                      I (AST_ocaml.mki (Type (ttype, [x])))
                    else 
                      I (AST_ocaml.mki (Type (Tok.unsafe_fake_tok "", [x])))
                  in
                  Some {
                    name = qualify name |> uniquify env kind;
                    kind = E.Type;
                    range = range any;
                  }
               | _ -> None
           )
      | _ -> []
  ) |> List.flatten

(*--------------------------------------------------*)
(* Generic part *)
(*--------------------------------------------------*)

let sanity_check _xs =
(*
  let group_by_basename =
    xs |> List.map (fun file -> Filename.basename file, file)
    |> Common.group_assoc_bykey_eff
  in
  group_by_basename |> List.iter (fun (_base, xs) ->
    if List.length xs > 1
    then pr2 (spf "multiple files with same name: %s" 
                     (xs |> Common.join "\n"))
  );
*)
  ()

let string_of_entity_kind kind =
  match kind with
  | E.Function  -> "function"
  | E.Global    -> "global"
  | E.Type      ->  if !lang = "c" then "enum"   else "type"
  | E.Class     -> if !lang = "c" then "struct" else "class"
  | E.Constant  -> "constant"
  (* old: was "function", because in C people sometimes use macro
   * where it could be a function (if C supported inline keywords),
   * so this is a low-level detail I wanted to hide.
   * But if you put "function" here then you destroy the work done
   * by uniquify and you can get a macro and function with the same
   * chunkname. So either put macro here, or change the call
   * to uniquify.
   *)
  | E.Macro     -> "macro" 

  | E.Exception -> "exception"
  | E.TopStmts  -> "toplevel"
  | E.Prototype -> "signature"
  | _ -> failwith (spf "not handled kind: %s" (E.string_of_entity_kind kind))

(*****************************************************************************)
(* OLD code *)
(*****************************************************************************)

(* OLD:
(* Help to create a first draft, to LPize a big set of files.
 * See now pfff -lpize which supports fine grained split of entities
 * (and also untabification) by actually parsing the source code files.
 *)
let lpize file =
  let files = Common.cat file |> Common.exclude (fun s -> s =~ "^[ \t]*$") in
  (* sanity check *)
  let group_by_basename =
    files |> List.map (fun file -> Filename.basename file, file)
      |> Common.group_assoc_bykey_eff
  in
  group_by_basename |> List.iter (fun (base, xs) ->
    if List.length xs > 1
    then pr2 (spf "multiple files with same name: %s" 
                     (xs |> Common.join "\n"))
  );

  let current_topdir = ref "" in
  files |> List.iter (fun file ->
    let xs = Common.split "/" file in
    let hd = List.hd xs in
    if hd <> !current_topdir
    then begin
      pr (spf "\\section{[[%s/]]}" hd);
      pr "";
      current_topdir := hd;
    end;

      pr (spf "\\subsection*{[[%s]]}" file);
      pr "";
    
    let base = (*Filename.basename*) file in
    pr (spf "<<%s>>=" base);
    Common.cat file |> List.iter (fun s ->
      pr (untabify s);
    );
    pr "@";
    pr "";
    pr "";

    (* for the initial 'make sync' to work *)
    Sys.command (spf "rm -f %s" file) |> ignore;
  )

*)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let lpize (xs : Fpath.t list) : unit = 

  (* C++ specifics. TODO: move elsewhere? when parse file? *)
(*
  Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
  let root = Sys.getcwd () in
  let local = Filename.concat root "pfff_macros.h" in
  if Sys.file_exists local
  then Parse_cpp.add_defs local;
*)

  sanity_check xs;
  let current_dir = ref "" in

  (* to avoid duped entities *)
  let hentities = Hashtbl.create 101 in

  xs |> List.iter (fun file ->
    let pr s = UConsole.print s in
    let dir = Filename.dirname !!file in
    if dir <> !current_dir
    then begin
      pr (spf "\\section{[[%s/]]}" dir);
      pr "";
      current_dir := dir;
    end;

    pr (spf "\\subsection*{[[%s]]}" !!file);
    pr "";

    let ast, toks = 
      (* CONFIG *)
        (* TODO: use a Pfff_or_tree_sitter approach again *)
        try 
          let res = Parse_ml.parse file in
          (* Parse_cpp.parse file   *)
          res.ast, res.tokens
        with Parsing_error.Syntax_error _x ->
            let toks = Parse_ml.tokens (Parsing_helpers.File file) in
            let res2 = Parse_ocaml_tree_sitter.parse file in
            (match res2.program with
            | Some ast -> ast, toks
            | None -> 
                failwith (spf "fail to parse %s also with tree-sitter" !!file)
            )
    in
    let env = {
      current_file = !!file;
      hentities = hentities;
      (* starts at 1 so that first have no int, just the filename
       * e.g.  function foo (foo.h), and then the second one have
       * function foo (foo.h)2
       *)
      cnt = ref 1;
    } in
    let entities : entity list =
      (* CONFIG *)
      extract_entities_ml env ast toks
      (* extract_entities_cpp env xs  *)
    in

    let hstart = 
      entities |> List.map (fun e -> fst e.range, e) |> Hashtbl_.hash_of_list
    in
    let hcovered = 
      entities |> List.map (fun e -> 
        let (lstart, lend) = e.range in
        Common2.enum_safe lstart lend
      ) |> List.flatten |> Hashtbl_.hashset_of_list
    in
    
    let lines = UFile.cat file in
    let arr = Array.of_list lines in

    (* CONFIG *)
    let suffix = "" in (* "arm" *)

    (* the chunks *)
    entities |> List.iter (fun e ->
        let (lstart, lend) = e.range in
        pr (spf "<<%s [[%s]]%s>>=" (string_of_entity_kind e.kind) e.name suffix);

        let nbdollars = ref 0 in
        Common2.enum_safe lstart lend |> List.iter (fun line ->
          let idx = line - 1 in
          if idx >= Array.length arr || idx < 0
          then failwith (spf "out of range for %s, line %d" !!file line);
          pr (untabify (arr.(line - 1)));
          nbdollars := !nbdollars + (count_dollar arr.(line - 1));
        );
        pr "@";
        if !nbdollars mod 2 =|= 1
        then pr "%$";
        pr "";
    );

    pr "";
    pr "%-------------------------------------------------------------";
    pr "";

    (* we don't use the basename (even though 'make sync' ' used to make
     * this assumption because) because we would have too many dupes.
     *)
    pr (spf "<<%s>>=" !!file);
    UFile.cat file |> List_.index_list_1 |> List.iter (fun (s, idx) ->
      match Common2.hfind_option idx hstart with
      | None -> 
          if Hashtbl.mem hcovered idx
          then ()
          else pr (untabify s)
      | Some e -> 
        pr (spf "<<%s [[%s]]%s>>" (string_of_entity_kind e.kind) e.name suffix);
    );
    pr "@";
    pr "";
    pr "";

    (* CONFIG *)
    (* for the initial 'make sync' to work *)
    (* Sys.command (spf "rm -f %s" file) |> ignore ; *)
  );
  ()
