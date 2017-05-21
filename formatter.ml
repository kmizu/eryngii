open Core.Std
open Located

module Op = struct

  type desc =
    | Nop
    | Text of string
    | Space of int
    | Newline of int
    | Indent of int
    | Dedent

  type t = {
    pos : int;
    desc : desc;
  }

  let create loc desc =
    { pos = Location.offset loc; desc }

  let length op =
    match op.desc with
    | Text s -> Some (String.length s)
    | Space len -> Some len
    | Newline len -> Some len
    | _ -> None

end

module Context = struct

  type t = {
    file : File.t;
    mutable ops : Op.t list;
    mutable indent : int list;
    mutable count : int option;
  }

  let create file =
    { file;
      ops = [];
      indent = [0];
      count = None;
    }

  let contents ctx =
    List.rev ctx.ops

  let clear ctx =
    ctx.ops <- []

  let start_count ctx =
    match ctx.count with
    | Some _ -> failwith "already start count"
    | None -> ctx.count <- Some 0

  let end_count ctx =
    match ctx.count with
    | None -> failwith "not start count"
    | Some count ->
      ctx.count <- None;
      count

  let count ctx =
    Option.value_exn ctx.count

  let add ctx op =
    ctx.ops <- op :: ctx.ops

  let add_string ctx loc text =
    add ctx @@ Op.create loc (Op.Text text)

  let add_text ctx text =
    add ctx @@ Op.create text.loc (Op.Text text.desc)

  let cur_indent ctx =
    List.hd_exn ctx.indent

  let indent ctx =
    (*cur_indent ctx |> spaces ctx *)
    ()

  let nest ?indent:size ctx =
    let size = (Option.value size ~default:4) + cur_indent ctx in
    ctx.indent <- size :: ctx.indent

  let unnest ctx =
    ctx.indent <- List.tl_exn ctx.indent

end

let rec parse ctx node =
  let open Ast_intf in
  let open Context in
  let open Located in
  let open Location in

  match node with
  | Module m ->
    List.iter m.module_decls ~f:(parse ctx)

  | Modname_attr attr ->
    add_text ctx attr.modname_attr_tag;
    (*
    modname_attr_open : token;
    modname_attr_name: text;
    modname_attr_close : token;
    modname_attr_dot : token;
     *)


  | Paren paren ->
    add_string ctx paren.enc_open "(";
    parse ctx paren.enc_desc;
    add_string ctx paren.enc_close ")"

  | Nop -> ()
  | _ -> ()

let finish _ctx =
  (* TODO *)
  ""

let format file node =
  let ctx = Context.create file in
  parse ctx node;
  finish ctx
