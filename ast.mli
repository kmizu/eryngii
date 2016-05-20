(* https://github.com/ignatov/intellij-erlang/blob/master/grammars/erlang.bnf *)

type op = op_desc Located.t

type op_desc =
  | Op_pos              (* "+" *)
  | Op_neg              (* "-" *)
  | Op_not              (* "not" *)
  | Op_lnot             (* "bnot" *)
  | Op_match            (* "=" *)
  | Op_send             (* "!" *)
  | Op_eq               (* "==" *)
  | Op_ne               (* "/=" *)
  | Op_le               (* "=<" *)
  | Op_lt               (* "<" *)
  | Op_ge               (* ">=" *)
  | Op_gt               (* ">" *)
  | Op_xeq              (* "=:=" *)
  | Op_xne              (* "=/=" *)
  | Op_list_add         (* "++" *)
  | Op_list_diff        (* "--" *)
  | Op_add              (* "+" *)
  | Op_sub              (* "-" *)
  | Op_mul              (* "*" *)
  | Op_div              (* "/" *)
  | Op_quo              (* "div" *)
  | Op_rem              (* "rem" *)
  | Op_and              (* "and" *)
  | Op_or               (* "or" *)
  | Op_xor              (* "xor" *)
  | Op_sand             (* "andalso" *)
  | Op_sor              (* "orelse" *)
  | Op_land             (* "band" *)
  | Op_lor              (* "bor" *)
  | Op_lxor             (* "bxor" *)
  | Op_lshift           (* "bsl" *)
  | Op_rshift           (* "bsr" *)

type ast = ast_desc located.t

type ast_desc =
  | Module_decl of ast list
  | Module of text attr
  | Export of type_spec list attr
  | Export_type of type_spec list attr
  | Import of type_spec list attr
  | On_load of type_spec list attr
  | Version of ast attr
  | Author of text attr
  | Fun_decl of fun_clause list
  | Guard_funccall of string * ast
  | Guard_bin of ast * bin_op * ast
  | Guard_unary of ast * unary_op
  | Guard_app of string * ast list
  | Guard_record of ast option * string * string
  | Guard_list of list_pat
  | Guard_tuple of ast list
  | Guard_paren_test of ast
  | Guard_paren of ast
  | Catch of ast
  | Block of ast
  | Body of ast list
  | If of (ast * ast) list (* guard, clause body *)
  | Case of ast * cr_clause list
  | Receive of cr_clause list * (ast * ast) option
  | Fun of ast
  | Fun_arity of ast option * ast * int
  | Query of ast
  | Unexp of ast * unary_op
  | Binexp of ast * bin_op * ast
  | Fun_call of fun_call
  | Fully_qual of ast * ast
  | Qual of text * text (* atom '.' atom *)
  | Record_creation of ast option * ast * ast
  | Record_access of ast option * ast * ast list
  | Paren of ast enclosed
  | Var of string
  | Universal (* "_" *)
  | Atom of string
  | Char of string
  | String of string
  | Int of int
  | Float of float
  | List_pat of list_pat
  | Tuple of elt_list enclosed
  | Record of ast * ast list (* record name, assocs *)
  | Assoc of ast * ast (* field name, value *)
  | List_comp of ast * ast list
  | Generator of ast * ast (* pattern, _exp *)

and 'a attr = {
  attr_minus : Location.t;
  attr_lparen : Location.t;
  attr_rparen : Location.t;
  attr_desc : 'a;
  attr_dot : Location.t;
}

and type_spec = {
  type_name : string;
  type_arity : int;
}

and fun_clause = {
  fun_clause_name : string option;
  fun_clause_patterns : ast list;
  fun_clause_guard : ast list;
  fun_clause_body : ast;
}

and fun_call = {
  fun_call_mod : ast option;
  fun_call_func : ast;
  fun_call_args : ast list;
}

and cr_clause = {
  cr_clause_pattern : ast;
  cr_clause_gaurd : ast list;
  cr_clause_body : ast;
}

and list_pat = {
  list_pat_head : ast list;
  list_pat_tail : ast option;
}

and text = string Located.t

and 'a enclosed = {
  enc_open : Location.t;
  enc_desc : 'a;
  enc_close : Location.t;
}

and alt_list = (ast, Location.t) Seplist.t
