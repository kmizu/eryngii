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
    | Lbin (* << *)
    | Rbin (* >> *)
    | Leveled_indent
    | Aligned_indent
    | Dedent
    | Semi
    | Comma
    | Dot
    | Rarrow

  type t = {
    pos : int;
    desc : desc;
  }

  let create pos desc =
    { pos; desc }

  let of_loc loc desc =
    { pos = Location.offset loc; desc }

  let space pos len =
    create pos (Space len)

  let length op =
    match op.desc with
    | Nop
    | Leveled_indent
    | Aligned_indent
    | Dedent -> None
    | Text s
    | Comment s -> Some (String.length s)
    | Space n
    | Newline n -> Some n
    | Lparen
    | Rparen
    | Lbrack
    | Rbrack
    | Lbrace
    | Rbrace
    | Semi
    | Comma
    | Dot -> Some 1
    | Lbin
    | Rbin
    | Rarrow -> Some 2

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
    | Newline n -> sprintf "nl(%d)" n
    | Lparen -> "'('"
    | Rparen -> "')'"
    | Lbrack -> "'['"
    | Rbrack -> "']'"
    | Lbrace -> "'{'"
    | Rbrace -> "'}'"
    | Lbin -> "'<<'"
    | Rbin -> "'>>'"
    | Semi -> "';'"
    | Comma -> "','"
    | Dot -> "'.'"
    | Rarrow -> "'->'"
    | Leveled_indent -> "l_indent"
    | Aligned_indent -> "a_indent"
    | Dedent -> "dedent"

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

  let string ctx loc text =
    add_loc ctx loc (Op.Text text)

  let text ctx text =
    add_loc ctx text.loc (Op.Text text.desc)

  let atom ctx atom =
    match atom with
    | `Unenclosed name ->
      text ctx name
    | `Enclosed name -> 
      string ctx name.loc "'";
      text ctx name;
      string ctx name.loc "'"

  let comment ctx text =
    let len = String.length text.desc in
    let buf = Buffer.create (len+1) in
    let body = String.lstrip text.desc ~drop:(fun c -> c = '%') in
    let sign = String.make (len - String.length body) '%'  in
    let body = String.strip body in
    Buffer.add_string buf sign;
    Buffer.add_string buf " ";
    Buffer.add_string buf body;
    add_loc ctx text.loc (Op.Comment (Buffer.contents buf))

  let space ctx loc n =
    add_loc ctx loc (Space n)

  let newline ctx loc n =
    add_loc ctx loc (Newline n)

  let lparen ctx loc =
    add_loc ctx loc Lparen

  let rparen ctx loc =
    add_loc ctx loc Rparen

  let lbrack ctx loc =
    add_loc ctx loc Lbrack

  let rbrack ctx loc =
    add_loc ctx loc Rbrack

  let lbrace ctx loc =
    add_loc ctx loc Lbrace

  let rbrace ctx loc =
    add_loc ctx loc Rbrace

  let lbin ctx loc =
    add_loc ctx loc Lbin

  let rbin ctx loc =
    add_loc ctx loc Rbin

  let semi ctx loc =
    add_loc ctx loc Semi

  let comma ctx loc =
    add_loc ctx loc Comma

  let dot ctx loc =
    add_loc ctx loc Dot

  let rarrow ctx loc =
    add_loc ctx loc Rarrow

  let indent ctx loc =
    add_loc ctx loc Leveled_indent

  let a_indent ctx loc =
    add_loc ctx loc Aligned_indent

  let dedent ctx loc =
    add_loc ctx loc Dedent

end

let sort ops =
  List.sort ops ~cmp:Op.(fun a b -> Int.compare a.pos b.pos)

let compact_newlines (ops:Op.t list) =
  Conf.debug "compact_newlines: [%s]"
    (String.concat (List.map ops ~f:Op.to_string) ~sep:", ");
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
  let _, _, rev_ops = List.fold_left ops ~init:(0, [0], [])
      ~f:(fun (col, depth, accu) op ->
          Conf.debug "count_indent: col %d: depth %d: %s"
            col (List.length depth) (Op.to_string op);
          match op.desc with
          | Lparen | Lbrack | Lbrace ->
            (col+1, col+1 :: depth, op :: accu)
          | Rparen | Rbrack | Rbrace | Semi | Dot ->
            (col+1, List.tl_exn depth, op :: accu)
          | Newline _ ->
            let indent = Op.create op.pos (Space (List.hd_exn depth)) in
            (0, depth, indent :: op :: accu)
          | Rarrow ->
            let size = List.hd_exn depth + 4 in
            (col+2, size :: depth, op :: accu)
          | Leveled_indent ->
            let size = List.hd_exn depth + 4 in
            (col, size :: depth, accu)
          | Aligned_indent ->
            (col, col :: depth, accu)
          | Dedent ->
            (col, List.tl_exn depth, accu)
          | Comment _ ->
            (col, depth, op :: accu)
          | _ ->
            let col = col + Option.value_exn (Op.length op) in
            (col, depth, op :: accu))
  in
  List.rev rev_ops

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
        | Lbin -> replace op.pos "<<"
        | Rbin -> replace op.pos ">>"
        | Semi -> replace op.pos ";"
        | Comma -> replace op.pos ","
        | Dot -> replace op.pos "."
        | Rarrow -> replace op.pos "->"
        | Nop 
        | Leveled_indent
        | Aligned_indent
        | Dedent -> ()
      );
  String.strip buf ^ "\n"

let parse_annots ctx =
  let open Context in

  List.iter (Annot.all ())
    ~f:(fun annot ->
        match annot with
        | Comment text -> comment ctx text
        (* TODO: count \r\n, \r, \n *)
        | Newline text -> newline ctx text.loc (String.length text.desc))

let rec parse_node ctx node =
  let open Ast_intf in
  let open Context in
  let open Located in
  let open Location in

  match node with
  | Module m ->
    List.iter m.module_decls ~f:(parse_node ctx)

  | Modname_attr attr ->
    text ctx attr.modname_attr_tag; (* -module *)
    indent ctx attr.modname_attr_tag.loc;
    lparen ctx attr.modname_attr_open;
    text ctx attr.modname_attr_name;
    rparen ctx attr.modname_attr_close;
    dot ctx attr.modname_attr_dot

  | Export_attr attr ->
    text ctx attr.export_attr_tag; (* -export *)
    indent ctx attr.export_attr_tag.loc;
    lparen ctx attr.export_attr_open;
    lbrack ctx attr.export_attr_fun_open;
    parse_fun_sigs ctx attr.export_attr_funs;
    rbrack ctx attr.export_attr_fun_close;
    rparen ctx attr.export_attr_close;
    dot ctx attr.export_attr_dot

  | Spec_attr attr ->
    text ctx attr.spec_attr_tag; (* -spec *)
    indent ctx attr.spec_attr_tag.loc;
    space ctx attr.spec_attr_tag.loc 1;
    begin match attr.spec_attr_mname with
      | None -> ()
      | Some (mname, colon) ->
        text ctx mname;
        string ctx colon ":"
    end;
    text ctx attr.spec_attr_fname;
    dedent ctx attr.spec_attr_fname.loc;
    a_indent ctx attr.spec_attr_fname.loc;

    Seplist.iter attr.spec_attr_clauses
      ~f:(fun sep clause ->
          (* TODO: guard *)
          lparen ctx clause.spec_clause_open;
          Option.iter clause.spec_clause_args ~f:(fun args ->
              Seplist.iter args ~f:(fun sep arg->
                  parse_spec_type ctx arg;
                  Option.iter sep ~f:(fun sep ->
                      string ctx sep ", ")
                ));
          rparen ctx clause.spec_clause_close;
          space ctx clause.spec_clause_close 1;
          rarrow ctx clause.spec_clause_arrow;
          space ctx clause.spec_clause_arrow 1;
          parse_spec_type ctx clause.spec_clause_return;
          Option.iter sep ~f:(semi ctx));

    dot ctx attr.spec_attr_dot;
    dedent ctx attr.spec_attr_dot

  | Fun_decl decl ->
    parse_fun_body ctx decl.fun_decl_body;
    dot ctx decl.fun_decl_dot

  | Call call ->
    lparen ctx call.call_open;
    begin match call.call_fname.fun_name_mname,
                call.call_fname.fun_name_colon with
    | Some name, Some colon ->
      parse_node ctx name;
      string ctx colon ":"
    | _ -> ()
    end;
    parse_node ctx call.call_fname.fun_name_fname;
    parse_node_list ctx call.call_args;
    rparen ctx call.call_close

  | Binexp e ->
    parse_node ctx e.binexp_left;
    space ctx e.binexp_op.loc 1;
    parse_op ctx e.binexp_op;
    space ctx e.binexp_op.loc 1;
    parse_node ctx e.binexp_right

  | Paren paren ->
    lparen ctx paren.enc_open;
    parse_node ctx paren.enc_desc;
    rparen ctx paren.enc_close

  | Var name ->
    text ctx name

  | Atom name ->
    atom ctx name

  | List list ->
    lbrack ctx list.list_open;
    parse_node_list ctx list.list_head;
    begin match list.list_bar, list.list_tail with
      | Some bar, Some tail ->
        string ctx bar " | ";
        parse_node ctx tail
      | _ -> ()
    end;
    rbrack ctx list.list_close

  | Nop -> ()
  | _ -> ()

and parse_fun_sigs ctx fsigs =
  let open Context in
  Seplist.iter fsigs
    ~f:(fun sep fsig ->
        parse_fun_sig ctx fsig;
        Option.iter sep ~f:(fun sep ->
            string ctx sep ", "))

and parse_fun_sig ctx fsig =
  let open Ast in
  let open Context in
  text ctx fsig.fun_sig_name;
  string ctx fsig.fun_sig_sep "/";
  text ctx fsig.fun_sig_arity

and parse_spec_type ctx spec =
  let open Ast in
  let open Context in
  match spec with
  | Spec_type.Paren paren ->
    lparen ctx paren.enc_open;
    parse_spec_type ctx paren.enc_desc;
    rparen ctx paren.enc_close

  | Named named ->
    begin match (named.named_module, named.named_colon) with
      | Some mname, Some colon ->
        text ctx mname;
        string ctx colon ":"
      | _ -> ()
    end;
    text ctx named.named_name;
    lparen ctx named.named_open;
    Option.iter named.named_args ~f:(fun args ->
        Seplist.iter args
          ~f:(fun sep arg ->
              parse_spec_type ctx arg;
              Option.iter sep ~f:(fun sep -> string ctx sep ", ")));
    rparen ctx named.named_close

  | Atom name ->
    atom ctx name

  | List spec ->
    lbrack ctx spec.enc_open;
    parse_spec_type ctx spec.enc_desc;
    rbrack ctx spec.enc_close

  | Union spec ->
    parse_spec_type ctx spec.union_left;
    string ctx spec.union_op " | ";
    parse_spec_type ctx spec.union_right

  | _ -> ()

and parse_fun_body ctx body =
  let open Context in
  Seplist.iter body
    ~f:(fun sep clause ->
        parse_fun_clause ctx clause;
        Option.iter sep ~f:(fun sep ->
            semi ctx sep))

and parse_fun_clause ctx clause =
  let open Context in
  Option.iter clause.fun_clause_name ~f:(text ctx);
  lparen ctx clause.fun_clause_open;
  parse_node_list ctx clause.fun_clause_ptns;
  rparen ctx clause.fun_clause_close;
  space ctx clause.fun_clause_close 1;
  begin match clause.fun_clause_when, clause.fun_clause_guard with
    | Some when_, Some guard ->
      string ctx when_ "when";
      space ctx when_ 1;
      parse_guard ctx guard;
      space ctx clause.fun_clause_arrow 1;
    | _ -> ()
  end;
  rarrow ctx clause.fun_clause_arrow;
  space ctx clause.fun_clause_arrow 1;
  parse_node_list ctx clause.fun_clause_body;
  ()

and parse_guard ctx guard =
  let open Context in
  Seplist.iter guard
    ~f:(fun sep es ->
        parse_node_list ctx es;
        Option.iter sep ~f:(fun sep ->
            semi ctx sep));

and parse_node_list ctx es =
  let open Context in
  Seplist.iter es
    ~f:(fun sep e ->
        parse_node ctx e;
        Option.iter sep ~f:(fun sep ->
            comma ctx sep;
            space ctx sep 1))

and parse_op ctx op =
  let open Context in
  let s = match op.desc with
    | Op_pos       -> "+"
    | Op_neg       -> "-"
    | Op_not       -> "not"
    | Op_lnot      -> "bnot"
    | Op_eq        -> "="
    | Op_ep        -> "!"
    | Op_eqq       -> "=="
    | Op_ne        -> "/="
    | Op_le        -> "=<"
    | Op_lt        -> "<"
    | Op_ge        -> ">="
    | Op_gt        -> ">"
    | Op_xeq       -> "=:="
    | Op_xne       -> "=/="
    | Op_list_add  -> "++"
    | Op_list_diff -> "--"
    | Op_add       -> "+"
    | Op_sub       -> "-"
    | Op_mul       -> "*"
    | Op_div       -> "/"
    | Op_quo       -> "div"
    | Op_rem       -> "rem"
    | Op_and       -> "and"
    | Op_andalso   -> "andalso"
    | Op_or        -> "or"
    | Op_orelse    -> "orelse"
    | Op_xor       -> "xor"
    | Op_sand      -> "andalso"
    | Op_sor       -> "orelse"
    | Op_land      -> "band"
    | Op_lor       -> "bor"
    | Op_lxor      -> "bxor"
    | Op_lshift    -> "bsl"
    | Op_rshift    -> "bsr"
  in
  string ctx op.loc s

let format file node =
  let ctx = Context.create file in
  parse_annots ctx;
  parse_node ctx node;
  let len, ops =
    List.rev ctx.ops
    |> sort
    |> compact_newlines
    |> count_indent
    |> compact_pos
  in
  Conf.debug "[%s]" (String.concat (List.map ops ~f:Op.to_string) ~sep:", ");
  write len ops
