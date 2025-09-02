
type t

val to_hex: t -> string
val from_hex: string -> t

val signature_of_string: string -> t

val signaturefile_of_file: string (* Common.filename *) -> string (* Common.filename*)
val parse_signaturefile: string (* Common.filename *) -> (string (* key *) * t) list


