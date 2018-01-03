
type orig = Web.t
type view = Code.t

(* main entry *)
val sync: lang:Lang.mark_language -> orig -> view -> orig
