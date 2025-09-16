
type t

val to_hex: t -> string
val from_hex: string -> t

val signature_of_string: string -> t

val signaturefile_of_file: Fpath.t -> Fpath.t
val parse_signaturefile: Fpath.t -> (string (* key *) * t) list


