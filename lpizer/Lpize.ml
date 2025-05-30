open Common
module E = Entity_code

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

(* for lpification, to get a list of files and handling the skip list *)
let find_source xs =
  let root = Common2_.common_prefix_of_files_or_dirs xs in
  let root = Unix.realpath root |> Common2_.chop_dirsymbol in
  let files = 
    failwith "TODO: find_source use Find_generic in codegraph"
  in
    (* Find_source.files_of_dir_or_files ~lang:!lang xs in *)
  files |> List.iter (fun file ->
    Logs.info (fun m -> m "processing: %s" (Filename_.readable root file))
  )

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

(*--------------------------------------------------*)
(* C *)
(*--------------------------------------------------*)

(*
module Ast = Ast_cpp
open Ast_cpp
module E = Entity_code

(*
module PI = Parse_info
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
  let any_gen = Ocaml_to_generic.any any in
  let ii = 
    AST_generic_helpers.ii_of_any any_gen |> List.filter Tok.is_origintok 
  in
  let (min, max) = 
    Tok_range.min_max_toks_by_pos ii 
  in

  let if_not_estet_comment ii otherwise =
    if (not (is_estet_comment ii))
    then ii else otherwise
  in
  match Comment_code.comment_before hooks_for_comment_ml min toks,
        Comment_code.comment_after hooks_for_comment_ml max toks
  with
  | None, None -> min, max
  | Some ii, None -> if_not_estet_comment ii min, max
  | None, Some ii -> min, if_not_estet_comment ii max
  | Some i1, Some i2 -> 
          if_not_estet_comment i1 min, if_not_estet_comment i2 max

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
      match top.i with
      | Let(_i1, _, [LetClassic(
                              {lname= (name, _); 
                               lparams=[];
                               lbody=Function(_, _);
                               _
                              })]) ->
        let kind = E.Function in 
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]


      | Let(_i1, _,[LetClassic({lname=(name, _); lparams=[]; _})]) ->
        let kind = E.Constant in 
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]

      | Let(_i1, _, [LetClassic(
                              {lname=(name, _); lparams=_::_; _})]) ->
        let kind = E.Function in
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]

      | Exception(_, (name, _), _) ->
        let kind = E.Exception in
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]

      | Let(_i1, _, [LetPattern(PatUnderscore _, _)]) ->
        incr cnt;
        [{
          name = qualify (spf "_%d" !cnt);
          kind = E.TopStmts;
          range = range (I top);
        }]

      | Type(_, [TyDecl({tname = (name, _); _})]) ->
        let kind = E.Type in
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]

      | Val (_, (name, _), _) ->
        let kind = E.Prototype in
        [{
          name = qualify name |> uniquify env kind;
          kind;
          range = range (I top);
        }]

      | Type(_t1, _xs) -> failwith "TODO: Type"
(*
        let (xs, ys) = Either_.partition (fun x -> x) xs in
        let zipped = Common2.zip xs (t1::ys) in
        zipped |> Common.map_filter (fun (x, _tok1) ->
          match x with
          | (TyDef(_, Name((name, _)), _, _)) -> 
            let kind = E.Type in
            Some {
              name = qualify name |> uniquify env kind;
              kind = E.Type;
              range = range (TypeDeclaration x);
            }
          | _ -> None
        )
*)
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

(* main entry point *)
let lpize xs = 

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
    let dir = Filename.dirname file in
    if dir <> !current_dir
    then begin
      pr (spf "\\section{[[%s/]]}" dir);
      pr "";
      current_dir := dir;
    end;

    pr (spf "\\subsection*{[[%s]]}" file);
    pr "";

    let ast, toks = 
      (* CONFIG *)
      let res = Parse_ml.parse (Fpath.v file) in
      (* Parse_cpp.parse file   *)
      res.ast, res.tokens
    in
    let env = {
      current_file = file;
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
    
    let lines = UFile.Legacy.cat file in
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
          then failwith (spf "out of range for %s, line %d" file line);
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
    pr (spf "<<%s>>=" file);
    UFile.Legacy.cat file |> List_.index_list_1 |> List.iter (fun (s, idx) ->
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
