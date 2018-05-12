open Common

(* this uses md5sum internally *)
type t = Digest.t

let to_hex a = Digest.to_hex a

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

let md5sum_of_string a =
  Common.profile_code "Signature.md5sum_of_string" (fun () ->
    (* old: Common2.md5sum_of_string a *)
    Digest.string a
  )
