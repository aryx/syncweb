open Common

module G = Graph_code
module E = Entity_code
module PI = Parse_info

let verbose = ref false
let output_dir = ref None

let dep_file_of_dir dir = 
  Filename.concat dir Graph_code.default_filename

let hook_def_node_for_c node g =
  let info = Graph_code.nodeinfo node g in
  let loc = info.G.pos in
  (match snd node with
  | E.Function -> 
    pr (spf "DEF:function:%s:%d:%s"
          loc.PI.file loc.PI.line (fst node))
  | _ -> ()
  )

let hook_use_edge_for_c src dst g loc =
  (match snd src, snd dst with
  | E.Function, E.Function ->
    pr (spf "USE:call:%s:%d:%s"
          loc.PI.file loc.PI.line
          (*(fst src) not needed *)
          (fst dst))
  | _ -> ()
  )
  


(* copy paste of pfff/main_codegraph.ml *)
let build_graph_code lang xs =
  let xs = List.map Common.realpath xs in
  let root, files = 
    match xs with
    | [root] -> 
        root, Find_source.files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = 
          Find_source.files_of_dir_or_files ~lang ~verbose:!verbose xs in
        root, files
  in

  let empty = Graph_code.empty_statistics () in
  let g, stats =
    try (
    match lang with
    | "ml"  -> 
      Graph_code_ml.build ~verbose:!verbose root files, empty
(*
    | "cmt"  -> 
          let ml_files = Find_source.files_of_root ~lang:"ml" root in
          let cmt_files = files in
          Graph_code_cmt.build ~verbose:!verbose ~root ~cmt_files ~ml_files, 
          empty
*)
    | "c" -> 
        Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
        let local = Filename.concat root "pfff_macros.h" in
        if Sys.file_exists local
        then Parse_cpp.add_defs local;

        Graph_code_c.hook_def_node := hook_def_node_for_c;
        Graph_code_c.hook_use_edge := (fun _ctx _in_assign (src, dst) g loc ->
          hook_use_edge_for_c src dst g loc;
        );

        Graph_code_c.build ~verbose:!verbose root files, empty
    | _ -> failwith ("language not supported: " ^ lang)
    )
    with Graph_code.Error err ->
      pr2 (Graph_code.string_of_error err);
      raise (Graph_code.Error err)
  in
  let output_dir = !output_dir ||| (Sys.getcwd()) in
  Graph_code.save g (dep_file_of_dir output_dir);
  (* Graph_code.print_statistics stats g; *)
  ()
