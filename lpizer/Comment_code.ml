(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to deal with comment tokens in code.
 *
 * See Lib_ast_fuzzy.token_kind
 *
 * todo: extract and factorize more from comment_php.ml
 *
 * history:
 *  - comment_php in pfff
 *  - comment_code.ml at some point in pfff
 *  - removed in semgrep, went to TODO-pfff-trimmage
 *  - restored for syncweb lpizer
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Use the Token_<lang>.ml helpers to put in those field
 * (e.g, Token_helpers_ml.{token_kind_of_tok,info_of_tok})
 *)
(* todo: duplicate of matcher/parse_fuzzy.ml *)
type 'tok hooks = {
  kind : 'tok -> Lib_ast_fuzzy.token_kind;
  tokf : 'tok -> Tok.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let drop_space_and_newline hooks toks =
  toks |> Common2.drop_while (fun t ->
      let kind = hooks.kind t in
      match kind with
      | PI.Esthet PI.Newline
      | PI.Esthet PI.Space ->
          true
      | _ -> false
  )

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

let comment_before hooks tok all_toks =
  let pos = Tok.bytepos_of_tok tok in
  let before =
    all_toks
    |> Common2.take_while (fun tok2 ->
           let info = hooks.tokf tok2 in
           let pos2 = Tok.bytepos_of_tok info in
           pos2 < pos)
  in
  let first_non_space =
    List.rev before |> drop_space_and_newline hooks
  in
  match first_non_space with
  | x :: _xs when hooks.kind x =*= PI.Esthet PI.Comment ->
      let info = hooks.tokf x in
      if Tok.col_of_tok info =|= 0 then Some info else None
  | _ -> None

let comment_after hooks tok all_toks =
  let pos = Tok.bytepos_of_tok tok in
  let line = Tok.line_of_tok tok in
  let after =
    all_toks
    |> Common2.drop_while (fun tok2 ->
           let info = hooks.tokf tok2 in
           let pos2 = Tok.bytepos_of_tok info in
           pos2 <= pos)
  in
  let first_non_space = after |> drop_space_and_newline hooks in
  match first_non_space with
  | x :: _xs when hooks.kind x =*= PI.Esthet PI.Comment ->
      let info = hooks.tokf x in
      (* for ocaml comments they are not necessarily in
       * column 0, but they must be just after
       *)
      if
        Tok.line_of_tok info =|= line || Tok.line_of_tok info =|= line + 1
        (* && PI.col_of_info info > 0 *)
      then Some info
      else None
  | _ -> None
