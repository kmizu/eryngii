open Core.Std

module Block = struct

  type t = {
    pos : int;
    size : int;
  }

  let create pos size =
    { pos; size }

end

module Token = struct

  type desc =
    | Nop
    | Text of string
    | Newlines of int
    | Begin of Block.t
    | End

  type token = {
    pos : int;
    desc : desc;
  }

  let create pos desc =
    { pos; desc }

  let text pos s =
    create pos (Text s)

  let spaces ?(len=1) pos =
    text pos (String.make len ' ')

  let newlines ?(len=1) pos =
    create pos (Newlines len)

  let length tok =
    match tok.desc with
    | Text s -> Some (String.length s)
    | Newlines n -> Some n
    | _ -> None

  let to_string tok =
    let open Printf in
    match tok.desc with
    | Nop -> "nop"
    | Text s -> sprintf "text(\"%s\")" s
    | Newlines n -> sprintf "nl(%d)" n
    | Begin block -> sprintf "begin(%d, %d)" block.pos block.size
    | End -> "end"

  let sort toks =
    List.sort toks ~cmp:(fun a b -> Int.compare a.pos b.pos)

  let write chan toks =
    let open Out_channel in
    let cur = ref (Block.create 0 0) in
    List.fold_left toks
      ~init:Nop
      ~f:(fun pre tok ->
          begin match pre, tok.desc with
            | Newlines _, Text s ->
              output_string chan s;
              let indent = !cur.pos + !cur.size in
              output_string chan (String.make indent '\n');
              tok.desc
            | Newlines _, _ -> tok.desc
            | _, Text s ->
              output_string chan s;
              tok.desc
            | _, Newlines n ->
              output_string chan (String.make n '\n');
              tok.desc
            | _ -> pre
          end)

end
