(* https://github.com/ignatov/intellij-erlang/blob/mter/grammars/erlang.bnf *)

type op = op_desc Located.t

and op_desc =
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

type t = desc Located.t

and desc =
  | Nop (* internal use *)
  | Module of module_
  | Module_attr of module_attr
  | Fun_decl of fun_body
  | Catch of token * t
  | Block of explist enclosed
  | If of if_
  | Case of case
  | Recv of recv
  | Try of try_
  | Anon_fun of anon_fun
  | Module_fun of module_fun
  | Query of t
  | Unexp of (op * t)
  | Binexp of binexp
  | Call of call
  | Qual of text * text (* atom '.' atom *)
  | Paren of t enclosed
  | Uscore
  | Var of text
  | Atom of text
  | Q_atom of text enclosed
  | Char of text
  | String of text
  | Int of text
  | Float of text
  | List of erl_list
  | Tuple of explist enclosed
  | Field of field
  | Update of update
  | List_compr of compr (* list comprehension *)
  | List_compr_gen of list_compr_gen
  | Map of map
  | Bits_compr of compr (* bitstring comprehension *)

and module_ = {
  module_attrs : stat list;
  module_decls : stat list;
}

and stat = t * token

and module_attr = {
  module_attr_minus : token;
  module_attr_tag : text;
  module_attr_open : token;
  module_attr_values : explist;
  module_attr_close : token;
}

and fun_body = (fun_clause, token) Seplist.t

and fun_clause = {
  fun_clause_name : text option;
  fun_clause_open : token;
  fun_clause_ptns : explist;
  fun_clause_close : token;
  fun_clause_when : token option;
  fun_clause_guard : explist option;
  fun_clause_arrow : token;
  fun_clause_body : explist;
}

and if_ = {
  if_begin : token;
  if_clauses : (if_clause, token) Seplist.t;
  if_end : token;
}

and if_clause = {
  if_clause_guard : explist;
  if_clause_arrow : token;
  if_clause_body : explist;
}

and case = {
  case_begin : token;
  case_exp : t;
  case_of : token;
  case_clauses : (cr_clause, token) Seplist.t;
  case_end : token;
}

and cr_clause = {
  cr_clause_ptn : t;
  cr_clause_when : token option;
  cr_clause_guard : explist;
  cr_clause_arrow : token;
  cr_clause_body : explist;
}

and recv = {
  recv_begin : token;
  recv_clauses : (cr_clause, token) Seplist.t;
  recv_after : recv_after option;
  recv_end : token;
}

and recv_after = {
  recv_after_begin : token;
  recv_after_timer : t;
  recv_after_arrow : token;
  recv_after_body : explist;
}

and try_ = {
  try_begin : token;
  try_of : token option;
  try_clauses : (try_clause, token) Seplist.t option;
  try_catch : try_catch;
}

and try_catch = {
  try_catch_begin : token option;
  try_catch_clauses : (try_clause, token) Seplist.t option;
  try_catch_after : try_catch_after option;
  try_catch_end : token;
}

and try_catch_after = {
  try_catch_after_begin : token;
  try_catch_after_exps : explist;
}

and try_clause = {
  try_clause_exn : (t * token) option;
  try_clause_exp : t;
  try_clause_guard : explist;
  try_clause_body : explist;
}

and anon_fun = {
  anon_fun_begin : token;
  anon_fun_body : fun_body;
  anon_fun_end : token;
}

and module_fun = {
  module_fun_prefix : token;
  module_fun_mname : t option;
  module_fun_colon : token option;
  module_fun_fname : t;
  module_fun_slash : token;
  module_fun_arity : t;
}

and fun_name = {
  fun_name_mname : t option;
  fun_name_colon : token option;
  fun_name_fname : t;
}

and call = {
  call_fname : fun_name;
  call_open : token;
  call_args : explist;
  call_close : token;
}

and binexp = {
  binexp_op : op;
  binexp_left : t;
  binexp_right : t;
}

and erl_list = {
  list_open : token;
  list_head : explist;
  list_bar : token option;
  list_tail : t option;
  list_close : token;
}

and compr = {
  compr_open : token;
  compr_exp : t;
  compr_sep : token;
  compr_quals : explist;
  compr_close : token;
}

and list_compr_gen = {
  gen_ptn : t;
  gen_arrow : token;
  gen_exp : t;
}

and update = {
  update_exp : t option;
  update_sharp : token;
  update_name : text;
  update_open : token;
  update_assocs : (assoc, token) Seplist.t;
  update_close : token;
}

and assoc = {
  assoc_key : text;
  assoc_val : t;
  assoc_sep : token;
}

and field = {
  field_exp : t option;
  field_sharp : token;
  field_rname : text;
  field_sep : token;
  field_fname : text;
}

and map = {
  map_sharp : token;
  map_open : token;
  map_assocs : (map_assoc, token) Seplist.t;
  map_close : token;
}

and map_assoc = {
  map_assoc_key : t;
  map_assoc_val : t;
  map_assoc_sep : [`Put | `Update] Located.t;
}

and text = string Located.t

and 'a enclosed = {
  enc_open : token;
  enc_desc : 'a;
  enc_close : token;
}

and explist = (t, token) Seplist.t

and token = Location.t

let enclose open_ desc close = {
  enc_open = open_;
  enc_desc = desc;
  enc_close = close;
}

let nop = Located.create None Nop
