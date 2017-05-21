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
    add_string ctx attr.modname_attr_open "(";
    add_text ctx attr.modname_attr_name;
    add_string ctx attr.modname_attr_close ")";
    add_string ctx attr.modname_attr_dot "."

  | Paren paren ->
    add_string ctx paren.enc_open "(";
    parse ctx paren.enc_desc;
    add_string ctx paren.enc_close ")"

  | Nop -> ()
  | _ -> ()

let sort ops =
  List.sort ops ~cmp:Op.(fun a b -> Int.compare a.pos b.pos)

let compact_pos (ops:Op.t list) =
  let pos, ops = List.fold_left ops ~init:(0, [])
      ~f:(fun (pos, accu) op ->
          let op = { op with pos = pos } in
          let pos, op = match Op.length op with
            | None -> (pos, op)
            | Some len -> (pos + len, op)
          in
          (pos, op :: accu))
  in
  (pos, List.rev ops)

let write len (ops:Op.t list) =
  let buf = String.init (len+1) ~f:(fun _ -> ' ') in
  let replace pos s = 
    ignore @@ List.fold_left (String.to_list s)
      ~init:pos
      ~f:(fun pos c ->
          String.set buf pos c;
          pos + 1)
  in
  List.iter ops ~f:(fun op ->
      match op.desc with
      | Text s -> replace op.pos s
      | _ -> ()
    );
  buf

let format file node =
  let ctx = Context.create file in
  parse ctx node;
  let ops = sort ctx.ops in
  let len, ops = compact_pos ops in
  write len ops
