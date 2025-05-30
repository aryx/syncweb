
(* Use the Token_<lang>.ml helpers to put in those field
 * (e.g, Token_helpers_ml.{token_kind_of_tok,info_of_tok})
 *)
type 'tok hooks = {
  kind : 'tok -> Lib_ast_fuzzy.token_kind;
  tokf : 'tok -> Tok.t;
}

val comment_before : 'a hooks -> Tok.t -> 'a list -> Tok.t option

val comment_after : 'a hooks -> Tok.t -> 'a list -> Tok.t option
