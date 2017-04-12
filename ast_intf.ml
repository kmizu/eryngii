(* https://github.com/ignatov/intellij-erlang/blob/mter/grammars/erlang.bnf *)

type token = Location.t

type 'a node_list = ('a, token) Seplist.t

type text = string Located.t

type text_list = (text, token) Seplist.t

type 'a enclosed_lit = [`Unenclosed of 'a | `Enclosed of 'a]

type atom = text enclosed_lit

let text_of_atom atom =
  match atom with
  | `Unenclosed text
  | `Enclosed text -> text

type 'a enclosed = {
  enc_open : token;
  enc_desc : 'a;
  enc_close : token;
}

module Spec_type = struct

  type t =
    | Paren of t enclosed
    | Atom of atom
    | Int of text
    | Range of range
    | Nil (* TODO: remove *)
    | Named of named
    | Bits of bits
    | List of t option enclosed
    | Tuple of tuple
    | Fun of fun_
    | Map of map
    | Record of record
    | Union of union
    | Constraint of constr

  and range = {
    range_start : text;
    range_dot : token;
    range_end : text;
  }

  and bits = {
    bits_open : token;
    bits_start_uscore : text option;
    bits_start_colon : token option;
    bits_start_bits : text option;
    bits_comma : token option;
    bits_cont_uscore1 : text option;
    bits_cont_colon : token option;
    bits_cont_uscore2 : text option;
    bits_cont_mul : token option;
    bits_cont_bits : text option;
    bits_close : token;
  }

  and named = {
    named_module : text option;
    named_colon : token option;
    named_name : text;
    named_open : token;
    named_args : t node_list option;
    named_close : token;
  }

  and tuple = {
    tuple_open: token;
    tuple_elts: t node_list option;
    tuple_close : token;
  }

  and fun_ = {
    fun_tag : token;
    fun_open : token;
    fun_body : fun_body option;
    fun_close : token;
  }

  and fun_body = {
    fun_body_open : token;
    fun_body_args : [`None | `Dot of token | `Types of t node_list];
    fun_body_close : token;
    fun_body_arrow : token;
    fun_body_type : t;
  }

  and map = {
    map_nsign : token;
    map_open : token;
    map_pairs : pair node_list option;
    map_close : token;
  }

  and pair = {
    pair_left : t;
    pair_op : [`Mandatory of token | `Optional of token];
    pair_right : t;
  }

  and record = {
    rec_nsign : token;
    rec_name : text;
    rec_open : token;
    rec_fields : field node_list option;
    rec_close : token;
  }

  and field = {
    field_name : text;
    field_eq : token option;
    field_init : t option;
    field_colon : token;
    field_type : t;
  }

  and union = {
    union_left : t;
    union_op : token;
    union_right : t;
  }

  and constr = {
    constr_name : text;
    constr_colon : token;
    constr_type : t;
  }


end

type op = op_desc Located.t

and op_desc =
  | Op_pos              (* "+" *)
  | Op_neg              (* "-" *)
  | Op_not              (* "not" *)
  | Op_lnot             (* "bnot" *)
  | Op_eq               (* "=" *)
  | Op_ep               (* "!" *)
  | Op_eqq              (* "==" *)
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
  | Op_andalso          (* "andalso" *)
  | Op_or               (* "or" *)
  | Op_orelse           (* "orelse" *)
  | Op_xor              (* "xor" *)
  | Op_sand             (* "andalso" *)
  | Op_sor              (* "orelse" *)
  | Op_land             (* "band" *)
  | Op_lor              (* "bor" *)
  | Op_lxor             (* "bxor" *)
  | Op_lshift           (* "bsl" *)
  | Op_rshift           (* "bsr" *)

type t =
  | Nop (* internal use *)
  | Module of module_
  | Module_attr of module_attr
  | Modname_attr of modname_attr
  | Export_attr of export_attr
  | Export_type_attr of export_attr
  | Import_attr of import_attr
  | Include_attr of include_attr
  | Inclib_attr of inclib_attr
  | Spec_attr of spec_attr
  | Type_attr of type_attr
  | Opaque_attr of type_attr
  | Define_attr of define_attr
  | Behav_attr of behav_attr
  | Record_attr of record_attr
  | Flow_macro_attr of flow_macro_attr
  | Flow_attr of flow_attr
  | Fun_decl of fun_decl
  | Catch of token * t
  | Block of exp_list enclosed
  | If of if_
  | Case of case
  | Recv of recv
  | Try of try_
  | Anon_fun of anon_fun
  | Module_fun of module_fun
  | Unexp of (op * t)
  | Binexp of binexp
  | Call of call
  | Paren of t enclosed
  | Uscore of text
  | Var of text
  | Atom of atom
  | Char of text
  | String of text list
  | Int of text
  | Float of text
  | List of erl_list
  | Binary of exp_list enclosed
  | Binary_elt of binary_elt
  | Tuple of exp_list enclosed
  | Field of field
  | Update of update
  | List_compr of compr (* list comprehension *)
  | List_compr_gen of list_compr_gen
  | Binary_compr of compr (* binary comprehension *)
  | Binary_compr_gen of binary_compr_gen
  | Map of map
  | Macro of macro

and module_ = {
  module_decls : t list;
  module_eof : token;
}

and module_attr = {
  module_attr_minus : token;
  module_attr_tag : text;
  module_attr_open : token;
  module_attr_values : exp_list;
  module_attr_close : token;
  module_attr_dot : token;
}

and modname_attr = {
  modname_attr_tag : text;
  modname_attr_open : token;
  modname_attr_name: text;
  modname_attr_close : token;
  modname_attr_dot : token;
}

and export_attr = {
  export_attr_tag : text;
  export_attr_open : token;
  export_attr_fun_open : token;
  export_attr_funs : fun_sig node_list;
  export_attr_fun_close : token;
  export_attr_close : token;
  export_attr_dot : token;
}

and import_attr = {
  import_attr_tag : text;
  import_attr_open : token;
  import_attr_module : text;
  import_attr_comma : token;
  import_attr_fun_open : token;
  import_attr_funs : fun_sig node_list;
  import_attr_fun_close : token;
  import_attr_close : token;
  import_attr_dot : token;
}

and fun_sig = {
  fun_sig_name : text;
  fun_sig_sep : token;
  fun_sig_arity : text;
}

and include_attr = {
  include_attr_tag : text;
  include_attr_open : token;
  include_attr_file: text;
  include_attr_close : token;
  include_attr_dot : token;
}

and inclib_attr = {
  inclib_attr_tag : text;
  inclib_attr_open : token;
  inclib_attr_file: text;
  inclib_attr_close : token;
  inclib_attr_dot : token;
}

and spec_attr = {
  spec_attr_tag : text;
  spec_attr_mname : (text * token) option;
  spec_attr_fname : text;
  spec_attr_clauses : spec_clause node_list;
  spec_attr_dot : token;
}

and spec_clause = {
  spec_clause_open : token;
  spec_clause_args : Spec_type.t node_list option;
  spec_clause_close : token;
  spec_clause_arrow : token;
  spec_clause_return : Spec_type.t;
  spec_clause_guard : (token * guard) option;
}

and type_attr = {
  type_attr_tag : text;
  type_attr_name : text;
  type_attr_open : token;
  type_attr_args : Spec_type.t node_list option;
  type_attr_close : token;
  type_attr_colon : token;
  type_attr_type : Spec_type.t;
  type_attr_dot : token;
}

and define_attr = {
  def_attr_tag : text;
  def_attr_open : token;
  def_attr_name : define_name;
  def_attr_comma : token;
  def_attr_value : t;
  def_attr_close : token;
  def_attr_dot : token;
}

and define_name = {
  def_name : text;
  def_args : text_list enclosed option;
}

and behav_attr = {
  behav_attr_tag : text;
  behav_attr_open : token;
  behav_attr_name : text;
  behav_attr_close : token;
  behav_attr_dot : token;
}

and record_attr = {
  rec_attr_tag : text;
  rec_attr_open : token;
  rec_attr_name : text;
  rec_attr_comma : token;
  rec_attr_rec_open : token;
  rec_attr_fields : Spec_type.field node_list option;
  rec_attr_rec_close : token;
  rec_attr_close : token;
  rec_attr_dot : token;
}

and flow_macro_attr = {
  flow_macro_attr_tag_type : [`Undef | `Ifdef | `Ifndef];
  flow_macro_attr_tag : text;
  flow_macro_attr_open : token;
  flow_macro_attr_macro : text;
  flow_macro_attr_close : token;
  flow_macro_attr_dot : token;
}
and flow_attr = {
  flow_attr_tag_type : [`Else | `Endif];
  flow_attr_tag : text;
  flow_attr_dot : token;
}

and fun_decl = {
  fun_decl_body : fun_body;
  fun_decl_dot : token;
}

and fun_body = fun_clause node_list

and fun_clause = {
  fun_clause_name : text option;
  fun_clause_open : token;
  fun_clause_ptns : exp_list;
  fun_clause_close : token;
  fun_clause_when : token option;
  fun_clause_guard : guard option;
  fun_clause_arrow : token;
  fun_clause_body : exp_list;
}

and if_ = {
  if_begin : token;
  if_clauses : if_clause node_list;
  if_end : token;
}

and if_clause = {
  if_clause_guard : guard;
  if_clause_arrow : token;
  if_clause_body : exp_list;
}

and case = {
  case_begin : token;
  case_exp : t;
  case_of : token;
  case_clauses : cr_clause node_list;
  case_end : token;
}

and cr_clause = {
  cr_clause_ptn : t;
  cr_clause_when : token option;
  cr_clause_guard : guard;
  cr_clause_arrow : token;
  cr_clause_body : exp_list;
}

and recv = {
  recv_begin : token;
  recv_clauses : cr_clause node_list;
  recv_after : recv_after option;
  recv_end : token;
}

and recv_after = {
  recv_after_begin : token;
  recv_after_timer : t;
  recv_after_arrow : token;
  recv_after_body : exp_list;
}

and try_ = {
  try_begin : token;
  try_exps : exp_list; 
  try_of : token option;
  try_clauses : cr_clause node_list option;
  try_catch : try_catch;
}

and try_catch = {
  try_catch_begin : token option;
  try_catch_clauses : try_clause node_list option;
  try_catch_after : try_catch_after option;
  try_catch_end : token;
}

and try_catch_after = {
  try_catch_after_begin : token;
  try_catch_after_exps : exp_list;
}

and try_clause = {
  try_clause_exn : (atom * token) option;
  try_clause_exp : t;
  try_clause_guard : (token * guard) option;
  try_clause_arrow : token;
  try_clause_body : exp_list;
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

and guard = exp_list node_list

and fun_name = {
  fun_name_mname : t option;
  fun_name_colon : token option;
  fun_name_fname : t;
}

and call = {
  call_fname : fun_name;
  call_open : token;
  call_args : exp_list;
  call_close : token;
}

and binexp = {
  binexp_op : op;
  binexp_left : t;
  binexp_right : t;
}

and erl_list = {
  list_open : token;
  list_head : exp_list;
  list_bar : token option;
  list_tail : t option;
  list_close : token;
}

and compr = {
  compr_open : token;
  compr_exp : t;
  compr_sep : token;
  compr_quals : exp_list;
  compr_close : token;
}

and list_compr_gen = {
  gen_ptn : t;
  gen_arrow : token;
  gen_exp : t;
}

and binary_compr_gen = {
  bin_gen_ptn : t;
  bin_gen_arrow : token;
  bin_gen_exp : t;
}

and update = {
  update_exp : t option;
  update_sharp : token;
  update_name : text;
  update_open : token;
  update_assocs : assoc node_list;
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
  map_exp : t option;
  map_nsign : token;
  map_open : token;
  map_pairs : map_pair node_list option;
  map_close : token;
}

and map_pair = {
  map_pair_key : t;
  map_pair_op : [`New of token | `Update of token];
  map_pair_value : t;
}

and binary_elt = {
  bin_elt_val : t;
  bin_elt_colon : token option;
  bin_elt_size : text option;
  bin_elt_slash : token option;
  bin_elt_type : t option;
}

and macro = {
  macro_q : token;
  macro_name : text;
}

and exp_list = t node_list

let enclose open_ desc close = {
  enc_open = open_;
  enc_desc = desc;
  enc_close = close;
}
