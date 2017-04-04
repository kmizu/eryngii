open Core.Std

module Op = struct

  type t =
    | Indent
    | Dedent
    | Comment of string
    | Text of string
    | Space of int
    | Newline

  let write buf op =
    let open Buffer in
    match op with
    | Text s -> add_string buf s
    | Newline -> add_string buf "\n"
    | _ -> ()

end

module Context = struct

  type t = {
    mutable ops : Op.t list;
    mutable indent_lv : int;
  }

  let create () =
    { ops = []; indent_lv = 0 }

  let contents ctx =
    List.rev ctx.ops

  let add ctx op =
    ctx.ops <- op :: ctx.ops

  let text ctx s =
    add ctx @@ Op.Text s

  let word ctx s =
    text ctx s

  let space ctx =
    add ctx @@ Op.Space 1

  let newline ctx =
    add ctx Op.Newline

  let newlines ctx n =
    for i = 1 to n do
      add ctx Op.Newline
    done

  let indent ctx =
    ctx.indent_lv <- ctx.indent_lv + 1

end

let rec write ctx node =
  let open Ast_intf in
  let open Context in
  let open Located in

  let iter nodes =
    List.iter nodes ~f:(fun node -> write ctx node)
  in

  match node with
  | Module m -> iter m.module_decls

  | Modname_attr attr ->
    text ctx "-module(";
    text ctx attr.modname_attr_name.desc;
    text ctx ").";
    newlines ctx 2;

  | Export_attr attr ->
    Printf.printf "export: %d\n" (Seplist.length attr.export_attr_funs);
    text ctx "-export([";
    Seplist.iter attr.export_attr_funs
      ~f:(fun sep f ->
          text ctx f.fun_sig_name.desc;
          text ctx "/";
          text ctx f.fun_sig_arity.desc;
          match sep with
          | None -> ()
          | Some _ -> text ctx ", ");
    text ctx "]).";
    newline ctx;

(*
  | Module_attr of module_attr
  | Import_attr of import_attr
  | Include_attr of include_attr
  | Inclib_attr of inclib_attr
  | Spec_attr of spec_attr
  | Def_attr of def_attr
  | Fun_decl of fun_decl
*)
  | _ -> ()

let format node =
  let ctx = Context.create () in
  write ctx node;
  let buf = Buffer.create 2000 in
  List.iter (Context.contents ctx) ~f:(Op.write buf);
  Buffer.contents buf

