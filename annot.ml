open Core.Std

type t =
  | Comment of Ast.text
  | Newline of Ast.text

let g_annots : t list ref = ref []

let all () = !g_annots

let add annot =
  g_annots := annot :: !g_annots

let add_comment text =
  add @@ Comment text

let add_newline text =
  add @@ Newline text
