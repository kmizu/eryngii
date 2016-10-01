include Ast_intf

let enclose open_ desc close =
  { enc_open = open_;
    enc_desc = desc;
    enc_close = close }

let create desc =
  match desc with
  | List list ->
    Located.with_range list.list_open list.list_close desc
  | _ -> failwith "notimpl"

let simple_fun_name name =
  { fun_name_mname = None;
    fun_name_colon = None;
    fun_name_fname = name; }

