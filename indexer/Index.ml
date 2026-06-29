open Common
open Fpath_.Operators
module G = Graph_code
module E = Entity_code

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let verbose = ref false
(*let output_dir = ref None *)

let _dep_file_of_dir dir =
  Filename.concat dir !!(Graph_code.default_filename)

(* Longest common path-component prefix of the given directories, kept in the
 * same (relative or absolute) form as the arguments. For a single directory
 * this is the directory itself. Used as the graph root when indexing several
 * directories at once. *)
let common_dir_prefix (roots : Fpath.t list) : Fpath.t =
  (* drop a trailing empty segment coming from a trailing slash, e.g.
   * "rio/" -> ["rio"] instead of ["rio"; ""] *)
  let segs (r : Fpath.t) =
    match List.rev (Fpath.segs r) with
    | "" :: rest -> List.rev rest
    | _ -> Fpath.segs r
  in
  let rec common_two acc a b =
    match (a, b) with
    | x :: xs, y :: ys when String.equal x y -> common_two (x :: acc) xs ys
    | _ -> List.rev acc
  in
  match List.map segs roots with
  | [] -> failwith "common_dir_prefix: no directory given"
  | first :: rest ->
      let prefix = List.fold_left (fun acc s -> common_two [] acc s) first rest in
      (match prefix with
       (* no shared prefix (e.g. disjoint relative dirs): fall back to cwd *)
       | [] | [ "" ] -> Fpath.v "."
       | _ -> Fpath.v (String.concat "/" prefix))

(* special hooks *)
let hook_def_node node g =
  let info : G.nodeinfo = Graph_code.nodeinfo node g in
  let name = fst node in
  let loc : Loc.t = info.G.pos in
  let pos : Pos.t = loc.pos in
  let kind = E.string_of_entity_kind (snd node) in
  UConsole.print (spf "DEF:%s:%s:%d:%s" kind !!(pos.file) pos.line name)
  

let hook_use_edge _src dst _g (loc : Loc.t) =
  let name = fst dst in
  let pos : Pos.t = loc.pos in
  let kind = E.string_of_entity_kind (snd dst) in
  UConsole.print (spf "USE:%s:%s:%d:%s"  kind !!(pos.file) pos.line name)
          (*(fst src) not needed *)

  

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* copy paste of pfff/main_codegraph.ml *)
let build_graph_code lang (roots : Fpath.t list) =

  (* When given multiple directories, we use their common prefix as the
   * graph root so that each directory keeps a distinct (readable) prefix in
   * the graph, and we gather the source files from every directory.
   * For a single directory the common prefix is the directory itself, so
   * this preserves the original single-root behavior.
   *
   * We compute the prefix on the path components, preserving the (possibly
   * relative) form of the arguments. This matters because the root and the
   * source files found below must be in the same form for Filename_.readable
   * to relate them, and because the file paths reported by the indexer should
   * stay as the user typed them (e.g. "rio/snarf.c", not an absolute path)
   * so the generated defs_and_uses.list still matches the literate document.
   *)
  let root = common_dir_prefix roots in
  (* gather files from all the given roots *)
  let files_of_all_roots find_one = roots |> List.concat_map find_one in

  let empty = Graph_code.empty_statistics () in
  let _g, _stats =
    try (
    match lang with
    | "ml"  ->
      let files = failwith "TODO" in
      Graph_code_ml.build ~verbose:!verbose root files, empty
    | "cmt"  ->
      let ml_files = files_of_all_roots (Find_source.files_of_root ~lang:"ml") in
      let cmt_files = files_of_all_roots (Find_source.files_of_root ~lang:"cmt") in
      Graph_code_cmt.hook_def_node := hook_def_node;
      Graph_code_cmt.hook_use_edge := (fun (src, dst) g loc ->
          hook_use_edge src dst g loc;
      );
      Graph_code_cmt.build root ~cmt_files ~ml_files,
      empty
    | "c" ->
        let files =
           files_of_all_roots
             (Find_generic.files_of_root ~filter_file:(fun _file -> true)
                Lang.C) in
        let local = Filename.concat !!root "../../pfff_macros.h" in
        if Sys.file_exists local
        then Parse_cpp.add_defs (Fpath.v local);

        Graph_code_c.hook_def_node := hook_def_node;
        Graph_code_c.hook_use_edge := (fun _ctx _in_assign (src, dst) g loc ->
          hook_use_edge src dst g loc;
        );

        Graph_code_c.build root files, empty
    | _ -> failwith ("language not supported: " ^ lang)
    )
    with Graph_code.Error err ->
      UCommon.pr2 (Graph_code.string_of_error err);
      raise (Graph_code.Error err)
  in
(*
  let output_dir = !output_dir ||| (Sys.getcwd()) in
  Graph_code.save g (dep_file_of_dir output_dir);
  Graph_code.print_statistics stats g;
*)
  ()
