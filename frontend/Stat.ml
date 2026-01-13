(* Copyright 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Web

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lines of code (LOC) and lines of explanations (LOE) for a .nw file *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  mutable loc: int;
  mutable loe: int;
}

(* TODO? could have conf where decide what is a LOE, like counting
 * nested chunknames as LOE, counting tex command as LOE, etc.
 *)

let stat_of_web (orig : Web.t) : t =
  let loc = ref 0 in
  let loe = ref 0 in

  let rec aux1 = function
    | Tex xs ->
        xs |> List.iter (fun s ->
            (* TEX *)
            match s with
            | "" -> ()
            | s when s =~ "^%.*" -> ()
            | s when s =~ "^\\\\[tl] " -> ()
            (* for now I consider Tex commands \xxx as legit explanations as
             * they can be section names for example which usually help to
             * understand the code.
             *)
            | _ -> 
                Logs.debug (fun m -> m "LOE: %s" s);
                incr loe
        )
    | ChunkDef (_def, xs) ->
      xs |> List.iter aux2
  and aux2 = function
    | Code s -> 
        (* CODE *)
        (match s with
        | "" -> ()
        (* LATER? look if only space line? *)
        | _ -> 
              Logs.debug (fun m -> m "LOC: %s" s);
              incr loc
        )
    | ChunkName (s, _id) ->
        (* consider a nested chunkname as some form of explanation *)
        Logs.debug (fun m -> m "LOE: <<%s>>" s);
        incr loe
  in
  orig |> List.iter aux1;
  { loc = !loc; loe = !loe }
