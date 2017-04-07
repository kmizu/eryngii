open Core.Std

type t =
  | Comment of Ast.text

let all_annots : t list ref = ref []

let finish () =
  all_annots := List.rev !all_annots

let add annot =
  all_annots := annot :: !all_annots

let add_comment text =
  add @@ Comment text
