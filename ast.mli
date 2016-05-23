(* https://github.com/ignatov/intellij-erlang/blob/mter/grammars/erlang.bnf *)

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

type t = desc Located.t

type desc =
  | Module of seq
  | Module_attr of module_attr
  | Fun_decl of (fun_clause, token) Seplist.t
  | Catch of (token * t)
  | Block of seq enclosed
  | If of (t * t) list (* guard, clause body *)
  | Case of case
  | Receive of case_clause list * (t * t) option
  | Anon_fun of anon_fun
  | Module_fun of module_fun
  | Query of t
  | Unexp of (token * t)
  | Binexp of binexp
  | Call of fun_call
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
  | List_ptn of list_ptn
  | Tuple of seq enclosed
  | Record of record
  | Field of field
  | Update_record of t * record
  | List_compr of compr (* list comprehension *)
  | Map of map
  | Update_map of update_map
  | Bits_compr of compr (* bitstring comprehension *)

and module_attr = {
  mattr_minus : token;
  mattr_tag : text;
  mattr_open : token;
  mattr_value : t;
  mattr_close : token;
}

and fun_clause = {
  fun_clause_name : text option;
  fun_clause_open : token;
  fun_clause_ptns : seq;
  fun_clause_close : token;
  fun_clause_when : token option;
  fun_clause_guard : seq;
  fun_clause_arrow : token;
  fun_clause_body : seq;
}

and case = {
  case_begin : token;
  case_of : token;
  case_clauses : (case_clause, token) Seplist.t;
  case_end : token;
}

and case_clause = {
  case_clause_ptn : t;
  case_clause_close : token;
  case_clause_when : token option;
  case_clause_guard : seq;
  case_clause_arrow : token;
  case_clause_body : seq;
}

and anon_fun = {
  anon_fun_begin : token;
  anon_fun_body : fun_body;
  anon_fun_end : token;
}

and module_fun = {
  module_fun_prefix : token;
  module_fun_mname : text option;
  module_fun_colon : token;
  module_fun_fname : text;
  module_fun_slash : token;
  module_fun_arity : text;
}

and fun_name = {
  fun_name_mname : t option;
  fun_name_colon : token option;
  fun_name_fname : text;
}

and call = {
  call_fname : fun_name;
  call_open : token;
  call_args : seq;
  call_close : token;
}

and case_clause = {
  case_clause_ptntern : t;
  case_clause_gaurd : t list;
  case_clause_body : t;
}

and binexp = {
  binexp_op : token;
  binexp_left : t;
  binexp_right : t;
}

and list_ptn = {
  list_ptn_open : token;
  list_ptn_head : t list;
  list_ptn_bar : token;
  list_ptn_tail : t option;
  list_ptn_close : token;
}

and compr = {
  compr_open : token;
  compr_exp : t;
  compr_sep : token;
  compr_quals : seq;
  compr_close : token;
}

and record = {
  rec_sharp : token;
  rec_name : text;
  rec_open : token;
  rec_assocs : (rec_assoc, token) Seplist.t;
  rec_close : token;
}

and rec_assoc = {
  rec_assoc_key : t;
  rec_assoc_val : t;
  rec_assoc_sep : token;
}

and field = {
  field_exp : t;
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
  map_assoc_sep : [`Put | `Update] token;
}

and text = string Located.t

and 'a enclosed = {
  enc_open : token;
  enc_desc : 'a;
  enc_close : token;
}

and seq = (t, token) Seplist.t

and token = Location.t
