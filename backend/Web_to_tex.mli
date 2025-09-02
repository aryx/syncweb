
(* todo: codegraph info at some point? *)
val web_to_tex:
  Web.t -> Fpath.t (* output file *) -> 
  (Crossref_code.defs * Crossref_code.uses) ->
  unit
