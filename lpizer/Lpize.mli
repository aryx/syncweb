
type conf = {
  lang: Lang.t;
  suffix: string;
}

val lpize: conf -> Fpath.t list -> unit
