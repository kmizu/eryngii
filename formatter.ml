open Core.Std
open Located

module Op = struct

  type desc =
    | Nop
    | Text of string
    | Comment of string
    | Space of int
    | Newline of int
    | Lparen
    | Rparen
    | Lbrack
    | Rbrack
    | Lbrace
    | Rbrace
    | Dot
    | Indent of int
    | Fix_begin
    | Var_begin
    | End

  type t = {
    pos : int;
    desc : desc;
  }

  let create pos desc =
    { pos; desc }

  let of_loc loc desc =
    { pos = Location.offset loc; desc }

  let spaces pos len =
    create pos (Space len)

  let length op =
    match op.desc with
    | Nop
    | Fix_begin
    | Var_begin
    | End -> None
    | Text s
    | Comment s -> Some (String.length s)
    | Space n
    | Newline n
    | Indent n -> Some n
    | Lparen
    | Rparen
    | Lbrack
    | Rbrack
    | Lbrace
    | Rbrace
    | Dot -> Some 1

  let length_exn op =
    Option.value_exn (length op)

  let length_zero op =
    Option.value (length op) ~default:0

  let add_pos op len =
    { op with pos = op.pos + len }

  let add_pos_of op other =
    add_pos op @@ length_exn other

  let to_string op =
    let open Printf in
    match op.desc with
    | Nop -> "nop"
    | Text s -> sprintf "text(\"%s\")" s
    | Comment s -> sprintf "comment(\"%s\")" s
    | Space n -> sprintf "space(%d)" n
    | Newline n -> sprintf "newline(%d)" n
    | Lparen -> "'('"
    | Rparen -> "')'"
    | Lbrack -> "'['"
    | Rbrack -> "']'"
    | Lbrace -> "'{'"
    | Rbrace -> "'}'"
    | Dot -> "'.'"
    | Indent n -> sprintf "indent(%d)" n
    | Fix_begin -> "fix_begin"
    | Var_begin -> "var_begin"
    | End -> "end"

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

  let last_pos ctx =
    match List.hd ctx.ops with
    | None -> None
    | Some op -> Some op.pos

  let last_pos_exn ctx =
    Option.value_exn (last_pos ctx)

  let add ctx op =
    ctx.ops <- op :: ctx.ops

  let add_loc ctx loc desc =
    add ctx @@ Op.of_loc loc desc

  let add_string ctx loc text =
    add_loc ctx loc (Op.Text text)

  let add_text ctx text =
    add_loc ctx text.loc (Op.Text text.desc)

  let add_atom ctx atom =
    match atom with
    | `Unenclosed name ->
      add_text ctx name
    | `Enclosed name -> 
      add_string ctx name.loc "'";
      add_text ctx name;
      add_string ctx name.loc "'"

  let add_comment ctx text =
    let len = String.length text.desc in
    let buf = Buffer.create (len+1) in
    let body = String.lstrip text.desc ~drop:(fun c -> c = '%') in
    let sign = String.make (len - String.length body) '%'  in
    let body = String.strip body in
    Buffer.add_string buf sign;
    Buffer.add_string buf " ";
    Buffer.add_string buf body;
    add_loc ctx text.loc (Op.Comment (Buffer.contents buf))

  let add_space ctx loc n =
    add_loc ctx loc (Space n)

  let add_newline ctx loc n =
    add_loc ctx loc (Newline n)

  let add_lp ctx loc =
    add_loc ctx loc Lparen

  let add_rp ctx loc =
    add_loc ctx loc Rparen

  let add_lbk ctx loc =
    add_loc ctx loc Lbrack

  let add_rbk ctx loc =
    add_loc ctx loc Rbrack

  let add_lbe ctx loc =
    add_loc ctx loc Lbrace

  let add_rbe ctx loc =
    add_loc ctx loc Rbrace

  let add_dot ctx loc =
    add_loc ctx loc Dot

  let add_fix_begin ctx loc =
    add_loc ctx loc Fix_begin

  let add_var_begin ctx loc =
    add_loc ctx loc Var_begin

  let add_end ctx loc =
    add_loc ctx loc End

end

let parse_annots ctx =
  let open Context in

  List.iter (Annot.all ())
    ~f:(fun annot ->
        match annot with
        | Comment text -> add_comment ctx text
        (* TODO: count \r\n, \r, \n *)
        | Newline text -> add_newline ctx text.loc (String.length text.desc))

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
    add_fix_begin ctx attr.modname_attr_tag.loc;
    add_lp ctx attr.modname_attr_open;
    add_text ctx attr.modname_attr_name;
    add_rp ctx attr.modname_attr_close;
    add_dot ctx attr.modname_attr_dot;
    add_end ctx attr.modname_attr_dot

  | Export_attr attr ->
    add_text ctx attr.export_attr_tag;
    add_fix_begin ctx attr.export_attr_tag.loc;
    add_lp ctx attr.export_attr_open;
    add_lbk ctx attr.export_attr_fun_open;
    parse_fun_sigs ctx attr.export_attr_funs;
    add_rbk ctx attr.export_attr_fun_close;
    add_rp ctx attr.export_attr_close;
    add_dot ctx attr.export_attr_dot;
    add_end ctx attr.export_attr_dot

  | Spec_attr attr ->
    add_text ctx attr.spec_attr_tag;
    add_space ctx attr.spec_attr_tag.loc 1;
    begin match attr.spec_attr_mname with
      | None -> ()
      | Some (mname, colon) ->
        add_text ctx mname;
        add_string ctx colon ":"
    end;
    add_text ctx attr.spec_attr_fname;
    add_fix_begin ctx attr.spec_attr_fname.loc;

    Seplist.iter attr.spec_attr_clauses
      ~f:(fun sep clause ->
          (* TODO: guard *)
          add_lp ctx clause.spec_clause_open;
          Option.iter clause.spec_clause_args ~f:(fun args ->
              Seplist.iter args ~f:(fun sep arg->
                  parse_spec_type ctx arg;
                  Option.iter sep ~f:(fun sep ->
                      add_string ctx sep ", ")
                ));
          add_rp ctx clause.spec_clause_close;
          add_string ctx clause.spec_clause_arrow " -> ";
          parse_spec_type ctx clause.spec_clause_return;
          Option.iter sep ~f:(fun sep ->
              add_string ctx sep ","));

    add_dot ctx attr.spec_attr_dot;
    add_end ctx attr.spec_attr_dot

  | Paren paren ->
    add_lp ctx paren.enc_open;
    parse ctx paren.enc_desc;
    add_rp ctx paren.enc_close

  | Nop -> ()
  | _ -> ()

and parse_fun_sigs ctx fsigs =
  let open Context in
  let len = Seplist.length fsigs in
  Seplist.iter fsigs
    ~f:(fun sep fsig ->
        parse_fun_sig ctx fsig;
        Option.iter sep ~f:(fun sep ->
            add_string ctx sep ",";
            if len < 3 then
              add_space ctx sep 1
            else begin
              add_newline ctx sep 1
            end))

and parse_fun_sig ctx fsig =
  let open Ast in
  let open Context in
  add_text ctx fsig.fun_sig_name;
  add_string ctx fsig.fun_sig_sep "/";
  add_text ctx fsig.fun_sig_arity

and parse_spec_type ctx spec =
  let open Ast in
  let open Context in
  match spec with
  | Spec_type.Paren paren ->
    add_lp ctx paren.enc_open;
    parse_spec_type ctx paren.enc_desc;
    add_rp ctx paren.enc_close

  | Named named ->
    begin match (named.named_module, named.named_colon) with
      | Some mname, Some colon ->
        add_text ctx mname;
        add_string ctx colon ":"
      | _ -> ()
    end;
    add_text ctx named.named_name;
    add_lp ctx named.named_open;
    Option.iter named.named_args ~f:(fun args ->
        Seplist.iter args
          ~f:(fun sep arg ->
              parse_spec_type ctx arg;
              Option.iter sep ~f:(fun sep -> add_string ctx sep ", ")));
    add_rp ctx named.named_close

  | Atom atom ->
    add_atom ctx atom

  | List spec ->
    add_lbk ctx spec.enc_open;
    parse_spec_type ctx spec.enc_desc;
    add_rbk ctx spec.enc_close

  | _ -> ()

let sort ops =
  List.sort ops ~cmp:Op.(fun a b -> Int.compare a.pos b.pos)

let adjust_comments (ops:Op.t list) =
  List.fold_left ops
    ~init:[]
    ~f:(fun accu op ->
        match List.hd accu with
        | None -> op :: accu
        | Some (pre:Op.t) ->
          match op.desc with
          | Comment s ->
            begin match pre.desc with
              | Newline _ -> op :: accu
              | Indent _ ->
                let accu = op :: List.tl_exn accu in
                let space = Op.spaces op.pos 1 in
                let op = Op.add_pos op 1  in
                let pre = Op.add_pos_of pre op in
                pre :: op :: space :: List.tl_exn accu
              | _ ->
                let space = Op.spaces op.pos 1 in
                Op.add_pos_of op space :: space :: accu
            end
          | _ -> op :: accu)
  |> List.rev

let compact_newlines (ops:Op.t list) =
  (*Printf.printf "compact_newlines: [%s]\n" (String.concat (List.map ops ~f:Op.to_string) ~sep:", ");*)
  List.fold_left ops
    ~init:(None, [])
    ~f:(fun (count, accu) op ->
        match (count, op.desc) with
        | None, Newline _ -> (Some 1, accu)
        | None, _ -> (None, op :: accu)
        | Some _, Newline _ -> (Some 2, accu)
        | Some n, _ ->
          let nl = Op.create op.pos (Newline n) in
          (None, op :: nl :: accu))
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

let count_indent (ops:Op.t list) =
  let open Op in
  let rec count (ops, accu) col depth : (Op.t list * Op.t list) =
    match ops with
    | [] -> ([], accu)
    | op :: ops ->
      match op.desc with
      | Lparen
      | Lbrack
      | Lbrace ->
        let block = count (ops, op :: accu) (col + 1) (col + 1 :: depth) in
        count block col depth
      | Rparen
      | Rbrack
      | Rbrace ->
        (ops, op :: accu)
      | End ->
        (ops, accu)
      | Fix_begin ->
        let block = count (ops, accu) col (col + 4 :: depth) in
        count block col depth
      | Var_begin ->
        (* TODO *)
        let block = count (ops, accu) col (col :: depth) in
        count block col depth
      | Newline _ ->
        let indent = Op.create op.pos (Indent (List.hd_exn depth)) in
        count (ops, indent :: op :: accu) 0 depth
      | Comment _ ->
        count (ops, op :: accu) col depth
      | _ ->
        count (ops, op :: accu) (col + Op.length_zero op) depth
  in
  let _, accu = count (ops, []) 0 [0] in
  List.rev accu

let write len (ops:Op.t list) =
  let buf = String.make (len*2) ' ' in
  let replace pos s = 
    ignore @@ List.fold_left (String.to_list s)
      ~init:pos
      ~f:(fun pos c ->
          String.set buf pos c;
          pos + 1)
  in
  let replace_spaces pos len =
    replace pos (String.make len ' ')
  in

  List.iter ops
    ~f:(fun op ->
        match op.desc with
        | Text s
        | Comment s -> replace op.pos s
        | Newline n -> replace op.pos (String.make n '\n')
        | Space n -> replace_spaces op.pos n
        | Lparen -> replace op.pos "("
        | Rparen -> replace op.pos ")"
        | Lbrack -> replace op.pos "["
        | Rbrack -> replace op.pos "]"
        | Lbrace -> replace op.pos "{"
        | Rbrace -> replace op.pos "}"
        | Dot -> replace op.pos "."
        | Nop 
        | Indent _
        | Fix_begin
        | Var_begin
        | End -> ()
      );
  String.strip buf ^ "\n"

let format file node =
  let ctx = Context.create file in
  parse_annots ctx;
  parse ctx node;
  let len, ops =
    List.rev ctx.ops
    |> sort
    |> adjust_comments
    |> compact_newlines
    |> count_indent
    |> compact_pos
  in
  (*Printf.printf "[%s]\n" (String.concat (List.map ops ~f:Op.to_string) ~sep:", ");*)
  write len ops
