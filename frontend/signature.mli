
type t = Digest.t

val to_hex: t -> string

val md5sum_auxfile_of_file: Common.filename -> Common.filename

val re_md5sum_in_aux_file: Str.regexp

val md5sum_of_string: string -> t

