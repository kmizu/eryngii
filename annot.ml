open Core.Std

type t =
  | Comment of Ast.text

let g_annots : t list array ref = ref [||]

let all_annots () = !g_annots

let init len =
  g_annots := Array.create ~len []

let add i annot =
  Array.replace !g_annots i ~f:(fun old -> annot :: old)

let add_comment text =
  let i = Located.(Option.value_exn text.loc).start.line in
  add i @@ Comment text

let comment i =
  List.find_mapi
    (Array.get !g_annots i)
    ~f:(fun _ annot ->
        match annot with 
        | Comment text -> Some text)
