open Core.Std

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

let write buf node =
  (* TODO: location *)
  let open Located in
  match node.desc with
  | Module m ->
    Buffer.add_string buf "{\"type\"=\"module}"
  | _ -> () (* TODO *)
  
let to_string node =
  let buf = Buffer.create 16 in
  write buf node;
  Buffer.contents buf
