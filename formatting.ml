open Core.Std
open Located

module Token = struct

  type desc =
    | Nop
    | Text of string
    | Comment of string
    | Newline of int
    | Leveled_indent
    | Aligned_indent
    | Label of [`Fun] * int
    | Labeled_indent of [`Fun] * int
    | Dedent

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
    | Label _
    | Labeled_indent _
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
    | Larrow
    | Larrow2
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
    | Larrow -> "'<-'"
    | Larrow2 -> "'<='"
    | Rarrow -> "'->'"
    | Leveled_indent -> "l_indent"
    | Aligned_indent -> "a_indent"
    | Label _ -> "label"
    | Labeled_indent _ -> "b_indent"
    | Dedent -> "dedent"

end

type t = {
  indent_size : int;
}

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

  let last_loc_exn ctx =
    let pos = last_pos_exn ctx in
    let pos = { Position.line = 0; col = 0; offset = pos } in
    Location.create pos pos

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

  let erl_string ctx s =
    string ctx s.loc "\"";
    text ctx s;
    string ctx s.loc "\""

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

  let larrow ctx loc =
    add_loc ctx loc Larrow

  let larrow2 ctx loc =
    add_loc ctx loc Larrow2

  let rarrow ctx loc =
    add_loc ctx loc Rarrow

  let indent ctx loc =
    add_loc ctx loc Leveled_indent

  let a_indent ctx loc =
    add_loc ctx loc Aligned_indent

  let label ctx loc name =
    add_loc ctx loc (Label (name, 0))

  let b_indent ctx loc name extra =
    add_loc ctx loc (Labeled_indent (name, extra))

  let dedent ctx loc =
    add_loc ctx loc Dedent

  let dedent_last ctx =
    dedent ctx @@ last_loc_exn ctx

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
        | Some n, Newline nl -> (Some (n + nl), accu)
        | Some n, _ ->
          let n = min n 3 in
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
          let col, depth, accu = match op.desc with
            | Lparen | Lbrack | Lbrace | Lbin ->
              (col+1, col+1 :: depth, op :: accu)
            | Rparen | Rbrack | Rbrace | Rbin | Semi ->
              (col+1, List.tl_exn depth, op :: accu)
            | Larrow | Larrow2 | Rarrow ->
              let size = List.hd_exn depth + 4 in
              (col+2, size :: depth, op :: accu)
            | Newline _ ->
              let size = List.hd_exn depth in
              let indent = Op.create op.pos (Space size) in
              (size, depth, indent :: op :: accu)
            | Leveled_indent ->
              let size = List.length depth * 4 in
              (col, size :: depth, accu)
            | Aligned_indent ->
              (col, col :: depth, accu)
            | Labeled_indent (name, extra) ->
              let found = List.find_map accu ~f:(fun op ->
                  match op.desc with
                  | Label (name2, base) when name = name2 ->
                    let size = base + extra in
                    let indent = Op.create op.pos (Space size) in
                    Some (col, size :: depth, indent :: accu)
                  | _ -> None)
              in
              begin match found with
                | None -> failwith "labeled indent not found"
                | Some accu -> accu
              end
            | Dedent ->
              (col, List.tl_exn depth, accu)
            | Label (name, _) ->
              let op = Op.create op.pos (Label (name, col)) in
              (col, depth, op :: accu)
            | Comment _ ->
              (col, depth, op :: accu)
            | _ ->
              let col = col + Option.value_exn (Op.length op) in
              (col, depth, op :: accu)
          in
          Conf.debug "count_indent: col %d: depth %d: %s"
            col ((List.length depth) - 1) (Op.to_string op);
          (col, depth, accu))
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
        | Larrow -> replace op.pos "<-"
        | Larrow2 -> replace op.pos "<="
        | Rarrow -> replace op.pos "->"
        | Nop 
        | Leveled_indent
        | Aligned_indent
        | Label _
        | Labeled_indent _
        | Dedent -> ()
      );
  String.strip buf ^ "\n"
