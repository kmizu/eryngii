include Ast_intf

let enclose open_ desc close =
  { enc_open = open_;
    enc_desc = desc;
    enc_close = close }
