open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* alternatives:
 * - camlgl ?
 * - ?
 *)



(* 
 * ref: Jon Harrop article and books
 *)

(* todo: take stuff from otimetracker *)



(* src: Jon Harrop *)
let pi = 4. *. atan 1.

let rec nest n f x =
  match n with
  | 0 -> x
  | n -> nest (n - 1) f (f x)


