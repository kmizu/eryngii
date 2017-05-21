open Core.Std
open Located

module Op = struct

  type desc =
    | Nop
    | Text of string
    | Comment of string
    | Space of int
    | Newline
    | Indent of int
    | Dedent

  type t = {
    pos : int;
    desc : desc;
  }

  let create pos desc =
    { pos; desc }

  let of_loc loc desc =
    { pos = Location.offset loc; desc }

  let length op =
    match op.desc with
    | Nop
    | Indent _
    | Dedent -> None

    | Text s
    | Comment s -> Some (String.length s)
    | Space len -> Some len
    | Newline -> Some 1

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
    add ctx @@ Op.of_loc loc (Op.Text text)

  let add_text ctx text =
    add ctx @@ Op.of_loc text.loc (Op.Text text.desc)

  let add_comment ctx text =
    let len = String.length text.desc in
    let buf = Buffer.create (len+1) in
    let body = String.lstrip text.desc ~drop:(fun c -> c = '%') in
    let sign = String.make (len - String.length body) '%'  in
    let body = String.strip body in
    Buffer.add_string buf sign;
    Buffer.add_string buf " ";
    Buffer.add_string buf body;
    add ctx @@ Op.of_loc text.loc (Op.Comment (Buffer.contents buf))

  let add_newline ctx text =
    add ctx @@ Op.of_loc text.loc Op.Newline

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

let parse_annots ctx =
  let open Context in

  List.iter (Annot.all ())
    ~f:(fun annot ->
        match annot with
        | Comment text -> add_comment ctx text
        | Newline text -> add_newline ctx text)

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

let adjust_comments (ops:Op.t list) =
  List.fold_left ops
    ~init:[]
    ~f:(fun accu op ->
        match op.desc with
        | Comment s ->
          begin match List.hd accu with
            | None -> op :: accu
            | Some (pre:Op.t) ->
              match pre.desc with
              | Op.Newline -> op :: accu
              | _ ->
                let space = Op.create op.pos (Space 1) in
                { op with pos = op.pos + 1 } :: space :: accu
          end
        | _ -> op :: accu)
  |> List.rev

let compact_newlines (ops:Op.t list) =
  List.fold_left ops
    ~init:(0, [])
    ~f:(fun (count, accu) op ->
        match op.desc with
        | Newline when count > 1 -> (count + 1, accu)
        | Newline -> (count + 1, op :: accu)
        | _ -> (0, op :: accu))
  |> snd
  |> List.rev

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
  let buf = String.make (len+1) ' ' in
  let replace pos s = 
    ignore @@ List.fold_left (String.to_list s)
      ~init:pos
      ~f:(fun pos c ->
          String.set buf pos c;
          pos + 1)
  in
  List.iter ops ~f:(fun op ->
      match op.desc with
      | Text s
      | Comment s -> replace op.pos s
      | Space n -> replace op.pos (String.make n ' ')
      | Newline -> String.set buf op.pos '\n'
      | _ -> failwith "not impl"
    );
  buf

let format file node =
  let ctx = Context.create file in
  parse_annots ctx;
  parse ctx node;
  let len, ops =
    sort ctx.ops
    |> adjust_comments
    |> compact_newlines
    |> compact_pos
  in
  write len ops
