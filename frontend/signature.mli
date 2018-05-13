
type t

val to_hex: t -> string
val from_hex: string -> t

val signature_of_string: string -> t

val signaturefile_of_file: Common.filename -> Common.filename
val parse_signaturefile: Common.filename -> (string (* key *) * t) list


