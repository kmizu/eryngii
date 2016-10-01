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
