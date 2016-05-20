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
  | Module of (t, Location.t) Seplist.t
  | Module_attr of module_attr
  | Fun_decl of fun_clause list
  | Catch of (Location.t * t)
  | Block of exp_list enclosed
  | If of (t * t) list (* guard, clause body *)
  | Case of t * cr_clause list
  | Receive of cr_clause list * (t * t) option
  | Anon_fun of anon_fun
  | Module_fun of module_fun
  | Query of t
  | Unexp of (Location.t * t)
  | Binexp of binexp
  | Call of fun_call
  | Qual of text * text (* atom '.' atom *)
  | Record_creation of t option * t * t
  | Record_access of t option * t * t list
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
  | Tuple of exp_list enclosed
  | Record of t * t list (* record name, assocs *)
  | Assoc of t * t (* field name, value *)
  | List_compr of compr (* list comprehension *)
  | Map of map
  | Bits_compr of compr (* bitstring comprehension *)

and module_attr = {
  mattr_minus : Location.t;
  mattr_tag : text;
  mattr_open : Location.t;
  mattr_value : t;
  mattr_close : Location.t;
}

and type_spec = {
  type_name : string;
  type_arity : int;
}

and fun_clause = {
  fun_clause_name : text;
  fun_clause_body : fun_body;
  fun_clause_end : Location.t;
}

and fun_body = {
  fun_body_lparen: Location.t;
  fun_body_ptnterns : t list;
  fun_body_rparen: Location.t;
  fun_body_guard : t list;
  fun_body_body : t;
}

and anon_fun = {
  anon_fun_begin : Location.t;
  anon_fun_body : fun_body;
  anon_fun_end : Location.t;
}

and module_fun = {
  module_fun_prefix : Location.t;
  module_fun_mname : text option;
  module_fun_colon : Location.t;
  module_fun_fname : text;
  module_fun_slash : Location.t;
  module_fun_arity : text;
}

and fun_name = {
  fname_mname : t option;
  fname_colon : Location.t option;
  fname_fname : text;
}

and call = {
  call_fname : fun_name;
  call_open : Location.t;
  call_args : exp_list;
  call_close : Location.t;
}

and cr_clause = {
  cr_clause_ptntern : t;
  cr_clause_gaurd : t list;
  cr_clause_body : t;
}

and binexp = {
  binexp_op : Location.t;
  binexp_left : t;
  binexp_right : t;
}

and list_ptn = {
  list_ptn_open : Location.t;
  list_ptn_head : t list;
  list_ptn_bar : Location.t;
  list_ptn_tail : t option;
  list_ptn_close : Location.t;
}

and compr = {
  compr_open : Location.t;
  compr_exp : t;
  compr_sep : Location.t;
  compr_quals : exp_list;
  compr_close : Location.t;
}

and map = {
  map_sharp : Location.t;
  map_open : Location.t;
  map_assocs : assoc Seplist.t;
  map_close : Location.t;
}

and assoc = {
  assoc_key : t;
  assoc_val : t;
  assoc_sep : Location.t;
}

and text = string Located.t

and 'a enclosed = {
  enc_open : Location.t;
  enc_desc : 'a;
  enc_close : Location.t;
}

and exp_list = (t, Location.t) Seplist.t
