open Common
open Fpath_.Operators

(* this uses md5sum internally *)
type t = Digest.t

let to_hex   a = Digest.to_hex a
let from_hex a = Digest.from_hex a

let signature_of_string (a : string) : t =
    (* old: Common2.md5sum_of_string a
     * but really slower than Digest because it forks a process *)
    Digest.string a
[@@profiling]


let signaturefile_of_file (file : Fpath.t) : Fpath.t = 
  let (d,b) = Filename_.db_of_filename !!file in
  let oldformat = Filename_.filename_of_db (d, ".md5sum_" ^ b) in
  if Sys.file_exists oldformat
  then Fpath.v oldformat
  else 
    let (d,b,e) = Filename_.dbe_of_filename !!file in
    (* works better with codemap, and also mkmany in plan9 *)
    Filename_.filename_of_dbe (d, spf ".md5sum_%s_%s" b e, "") |> Fpath.v

let re_signature_in_signaturefile = Str.regexp
  "\\(.*\\) |\\(.*\\)$"

let (==~) s re =
    Str.string_match re s 0

let parse_signaturefile (sigfile : Fpath.t) =
  UFile.cat sigfile |> List.map (fun s -> 
    if s ==~ re_signature_in_signaturefile
    then 
      let (a,b) = Common.matched2 s in
      (a, from_hex b)
    else failwith ("wrong format in Signature.parse_signaturefile: " ^ s)
  )
