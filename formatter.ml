open Core.Std

module Op = struct

  type newline =
    | Export

  type t =
    | Nop
    | Indent
    | Dedent
    | Comment of string
    | Text of string
    | Space of int
    | Newline of newline option

  let write buf ops =
    let open Buffer in
    ignore @@ List.fold_left ~init:Nop ops ~f:(fun prev op ->
        begin match op with
          | Text s -> add_string buf s
          | Space n ->
            for i = 0 to n-1 do
              add_string buf " "
            done
          | Newline (Some tag) when op = prev -> ()
          | Newline (Some tag) -> add_string buf "\n"
          | Newline None -> add_string buf "\n"
          | _ -> ()
        end;
        op)

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

  let rarrow ctx =
    add ctx @@ Op.Text "->"

  let space ctx =
    add ctx @@ Op.Space 1

  let spaces ctx n =
    add ctx @@ Op.Space n

  let newline ?tag ctx =
    add ctx @@ Op.Newline tag

  let newlines ?tag ?(n=2) ctx =
    for i = 1 to n do
      add ctx @@ Op.Newline tag
    done

  let dot_newline ctx =
    text ctx ".";
    newline ctx

  let dot_newlines ?(n=2) ctx =
    text ctx ".";
    newlines ctx ~n

  let indent ctx =
    ctx.indent_lv <- ctx.indent_lv + 1;
    newline ctx

  let indent_spaces ctx =
    spaces ctx @@ ctx.indent_lv * 4

  let dedent ctx =
    ctx.indent_lv <- ctx.indent_lv - 1;
    newline ctx

end

let rec write ctx node =
  let open Ast_intf in
  let open Context in
  let open Located in

  let iter nodes =
    List.iter nodes ~f:(fun node -> write ctx node)
  in

  let write_sep opt sep =
    match opt with
    | None -> ()
    | Some _ -> text ctx sep
  in

  let write_seplist seplist sep =
    Seplist.iter seplist ~f:(fun sep_opt node ->
        write ctx node;
        write_sep sep_opt sep)
  in

  let write_when_guard w guard =
    match (w, guard) with
    | Some w, Some guard ->
      text ctx "when ";
      write_seplist guard ", ";
      text ctx " "
    | _ -> ()
  in

  let write_fun_sig fsig =
    text ctx fsig.fun_sig_name.desc;
    text ctx "/";
    text ctx fsig.fun_sig_arity.desc
  in

  let write_fun_sigs fsigs =
    Seplist.iter fsigs
      ~f:(fun sep fsig ->
          write_fun_sig fsig;
          write_sep sep ", ")
  in

  let write_pattern ptn =
    write ctx ptn
  in

  let write_patterns ptns =
    Seplist.iter ptns ~f:(fun sep ptn ->
        write_pattern ptn;
        write_sep sep ", ")
  in

  let write_fun_clause clause =
    Option.iter clause.fun_clause_name ~f:(fun name -> text ctx name.desc);
    text ctx "(";
    write_patterns clause.fun_clause_ptns;
    text ctx ") ";
    write_when_guard clause.fun_clause_when clause.fun_clause_guard;
    rarrow ctx;
    indent ctx;
    Seplist.iter clause.fun_clause_body ~f:(fun sep exp ->
        indent_spaces ctx;
        write ctx exp;
        write_sep sep ",\n");
  in

  let write_fun_body body =
    Seplist.iter body
      ~f:(fun sep clause ->
          write_fun_clause clause;
          write_sep sep ", ")
  in

  match node with
  | Module m -> iter m.module_decls

  | Modname_attr attr ->
    text ctx "-module(";
    text ctx attr.modname_attr_name.desc;
    text ctx ").";
    newlines ctx

  | Export_attr attr ->
    text ctx "-export([";
    write_fun_sigs attr.export_attr_funs;
    text ctx "]).";
    newline ctx;
    newline ctx ~tag:Export

  | Import_attr attr ->
    text ctx "-import(";
    text ctx attr.import_attr_module.desc;
    text ctx ", [";
    write_fun_sigs attr.import_attr_funs;
    text ctx "]).";
    newlines ctx

  | Fun_decl decl ->
    write_fun_body decl.fun_decl_body;
    dot_newline ctx;
    dedent ctx

  | Binexp exp ->
    write ctx exp.binexp_left;
    let op = match exp.binexp_op.desc with
      | Op_eq -> "="
      | _ -> "_?"
    in
    space ctx;
    text ctx op;
    space ctx;
    write ctx exp.binexp_right

  | Var name ->
    text ctx name.desc

  | Atom value
  | Int value ->
    text ctx value.desc

(*

  | Module_attr of module_attr
  | Include_attr of include_attr
  | Inclib_attr of inclib_attr
  | Spec_attr of spec_attr
  | Def_attr of def_attr
*)
  | _ -> ()

let format node =
  let ctx = Context.create () in
  write ctx node;
  let buf = Buffer.create 2000 in
  Op.write buf @@ Context.contents ctx;
  Buffer.contents buf

