open Common

module G = Graph_code
module E = Entity_code
module PI = Parse_info

(* copy paste of pfff/main_codegraph.ml *)

let find_source__files_of_dir_or_files ~lang xs = 
  match lang with
  | "cmt"  -> 
    Lib_parsing_ml.find_cmt_files_of_dir_or_files xs
  | _ -> Find_source.files_of_dir_or_files ~lang xs

let find_source__files_of_root ~lang root = 
  match lang with
  | "cmt"  -> 
    Lib_parsing_ml.find_cmt_files_of_dir_or_files [root]
  | _ -> Find_source.files_of_root ~lang root


let verbose = ref false
(*let output_dir = ref None *)

let dep_file_of_dir dir = 
  Filename.concat dir Graph_code.default_filename

(* special hooks *)
let hook_def_node node g =
  let info = Graph_code.nodeinfo node g in
  let name = fst node in
  let loc = info.G.pos in
  let kind = E.string_of_entity_kind (snd node) in
  pr (spf "DEF:%s:%s:%d:%s" kind loc.PI.file loc.PI.line name)
  

let hook_use_edge _src dst _g loc =
  let name = fst dst in
  let kind = E.string_of_entity_kind (snd dst) in
  pr (spf "USE:%s:%s:%d:%s"  kind loc.PI.file loc.PI.line name)
          (*(fst src) not needed *)

  


(* copy paste of pfff/main_codegraph.ml *)
let build_graph_code lang xs =
  let xs = List.map Common.fullpath xs in
  let root, files = 
    match xs with
    | [root] -> 
        root, find_source__files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = 
          find_source__files_of_dir_or_files ~lang xs in
        root, files
  in

  let empty = Graph_code.empty_statistics () in
  let _g, _stats =
    try (
    match lang with
    | "ml"  -> 
      Graph_code_ml.build ~verbose:!verbose root files, empty
    | "cmt"  -> 
      let ml_files = Find_source.files_of_root ~lang:"ml" root in
      let cmt_files = files in

        Graph_code_cmt.hook_def_node := hook_def_node;
        Graph_code_cmt.hook_use_edge := (fun (src, dst) g loc ->
          hook_use_edge src dst g loc;
        );

      Graph_code_cmt.build ~verbose:!verbose ~root ~cmt_files ~ml_files, 
      empty
    | "c" -> 
        Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
        let local = Filename.concat root "pfff_macros.h" in
        if Sys.file_exists local
        then Parse_cpp.add_defs local;

        Graph_code_c.hook_def_node := hook_def_node;
        Graph_code_c.hook_use_edge := (fun _ctx _in_assign (src, dst) g loc ->
          hook_use_edge src dst g loc;
        );

        Graph_code_c.build ~verbose:!verbose root files, empty
    | _ -> failwith ("language not supported: " ^ lang)
    )
    with Graph_code.Error err ->
      pr2 (Graph_code.string_of_error err);
      raise (Graph_code.Error err)
  in
(*
  let output_dir = !output_dir ||| (Sys.getcwd()) in
  Graph_code.save g (dep_file_of_dir output_dir);
  Graph_code.print_statistics stats g;
*)
  ()
