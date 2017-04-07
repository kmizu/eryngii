open Core.Std
open Located

module Comment = struct

  type t = {
    text : Ast.text;
    mutable used : bool;
  }

end

module Op = struct

  type t =
    | Nop
    | Indent
    | Dedent
    | Comment of string
    | Text of string
    | Space of int
    | Newline

  let write buf ops =
    let open Buffer in
    ignore @@ List.fold_left ~init:Nop ops ~f:(fun prev op ->
        begin match op with
          | Text s -> add_string buf s
          | Space n ->
            for i = 0 to n-1 do
              add_string buf " "
            done
          | Newline -> add_string buf "\n"
          | _ -> ()
        end;
        op)

end

module Context = struct

  type t = {
    buf : Buffer.t;
    mutable ops : Op.t list;
    mutable indent_lv : int;
    mutable comments : Comment.t list;
  }

  let create buf =
    let comments = List.filter_map !Annot.all_annots
        ~f:(fun annot ->
            match annot with
            | Comment text -> Some ({ Comment.text = text; used = false }))
    in
    { buf;
      ops = [];
      indent_lv = 0;
      comments;
    }

  let contents ctx =
    List.rev ctx.ops

  let add ctx op =
    ctx.ops <- op :: ctx.ops

  let text ctx s =
    add ctx @@ Op.Text s

  let word ctx s =
    text ctx s

  let atom ctx = function
    | `Unenclosed name ->
      text ctx name.desc
    | `Enclosed name ->
      text ctx "'";
      text ctx name.desc;
      text ctx "'"

  let rarrow ctx =
    add ctx @@ Op.Text "->"

  let space ctx =
    add ctx @@ Op.Space 1

  let spaces ctx n =
    add ctx @@ Op.Space n

  let newline ctx =
    add ctx @@ Op.Newline

  let newlines ?(n=2) ctx =
    for i = 1 to n do
      add ctx @@ Op.Newline
    done

  let dot_newline ctx =
    text ctx ".";
    newline ctx

  let dot_newlines ?(n=2) ctx =
    text ctx ".";
    newlines ctx ~n

  let indent ctx =
    ctx.indent_lv <- ctx.indent_lv + 1

  let indent_spaces ctx =
    spaces ctx @@ ctx.indent_lv * 4

  let dedent ctx =
    ctx.indent_lv <- ctx.indent_lv - 1

end

let write_comment ctx pos =
  let open Context in
  let open Located in
  let open Location in
  let open Position in
  let comments = List.filter ctx.comments
      ~f:(fun com ->
          let start = (Option.value_exn com.text.loc).start in
          not com.used && start.line < pos.line
        )
  in
  List.iter comments ~f:(fun com ->
      com.used <- true;
      text ctx com.text.desc;
      newline ctx);
  ()

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

  let write_exp_list exp_list =
    write_seplist exp_list ", "
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
    newline ctx;
    Seplist.iter clause.fun_clause_body ~f:(fun sep exp ->
        indent_spaces ctx;
        write ctx exp;
        write_sep sep ",\n")
  in

  let write_fun_body body =
    Seplist.iter body
      ~f:(fun sep clause ->
          write_fun_clause clause;
          write_sep sep ";\n")
  in

  let write_fun_name name =
    Option.iter name.fun_name_mname ~f:(write ctx);
    write ctx name.fun_name_fname
  in

  let write_op op =
    text ctx @@ match op with
    | Op_pos -> "+"
    | Op_neg -> "-"
    | Op_not -> "not"
    | Op_lnot -> "bnot"
    | Op_eq -> "="
    | Op_ep -> "!"
    | Op_eqq -> "=="
    | Op_ne -> "/="
    | Op_le -> "=<"
    | Op_lt -> "<"
    | Op_ge -> ">="
    | Op_gt -> ">"
    | Op_xeq -> "=:="
    | Op_xne -> "=/="
    | Op_list_add -> "++"
    | Op_list_diff -> "--"
    | Op_add -> "+"
    | Op_sub -> "-"
    | Op_mul -> "*"
    | Op_div -> "/"
    | Op_quo -> "div"
    | Op_rem -> "rem"
    | Op_and -> "and"
    | Op_andalso -> "andalso"
    | Op_or -> "or"
    | Op_orelse -> "orelse"
    | Op_xor -> "xor"
    | Op_sand -> "andalso"
    | Op_sor -> "orelse"
    | Op_land -> "band"
    | Op_lor -> "bor"
    | Op_lxor -> "bxor"
    | Op_lshift -> "bsl"
    | Op_rshift -> "bsr"
  in

  (* write comments *)
  let start_pos = Ast.start_pos node in
  Option.iter start_pos ~f:(write_comment ctx);

  match node with
  | Module m -> iter m.module_decls

  | Modname_attr attr ->
    text ctx "-module(";
    text ctx attr.modname_attr_name.desc;
    text ctx ").";
    newline ctx

  | Export_attr attr ->
    text ctx "-export([";
    write_fun_sigs attr.export_attr_funs;
    text ctx "]).";
    newline ctx

  | Import_attr attr ->
    text ctx "-import(";
    text ctx attr.import_attr_module.desc;
    text ctx ", [";
    write_fun_sigs attr.import_attr_funs;
    text ctx "]).";
    newline ctx

  | Define_attr attr ->
    text ctx "-define(";
    text ctx @@ Naming.uppercase attr.def_attr_name.desc;
    text ctx ", ";
    write ctx attr.def_attr_value;
    text ctx ").";
    newline ctx

  | Spec_attr attr ->
    let rec write_spec_type ty =
      let open Spec_type in
      match ty with
      | Atom (`Unenclosed name) ->
        text ctx name.desc
      | Atom (`Enclosed name) ->
        text ctx "'";
        text ctx name.desc;
        text ctx "'"
      | Int value ->
        text ctx value.desc
      | Nil ->
        text ctx "[]"
      | List ty ->
        text ctx "[";
        Option.iter ty.enc_desc ~f:write_spec_type;
        text ctx "]";
      | Named named ->
        text ctx named.named_name.desc;
        text ctx "(";
        Seplist.opt_iter named.named_args ~f:(fun sep arg ->
            write_spec_type arg;
            write_sep sep ", ");
        text ctx ")"
      | _ -> ()
    in

    text ctx "-spec ";
    begin match attr.spec_attr_mname with
      | None -> ()
      | Some (mname, _) ->
        text ctx mname.desc;
        text ctx ":"
    end;
    text ctx attr.spec_attr_fname.desc;
    Seplist.iter attr.spec_attr_clauses
      ~f:(fun sep clause ->
          text ctx "(";
          Seplist.opt_iter clause.spec_clause_args ~f:(fun sep arg ->
              write_spec_type arg;
              write_sep sep ", "); 
          text ctx ") -> ";
          write_spec_type clause.spec_clause_return;
          write_sep sep ";");
    dot_newline ctx

  | Fun_decl decl ->
    indent ctx;
    write_fun_body decl.fun_decl_body;
    text ctx ".";
    newlines ctx;
    dedent ctx

  | Call call ->
    write_fun_name call.call_fname;
    text ctx "(";
    write_exp_list call.call_args;
    text ctx ")"

  | Binexp exp ->
    write ctx exp.binexp_left;
    space ctx;
    write_op exp.binexp_op.desc;
    space ctx;
    write ctx exp.binexp_right

  | Paren paren ->
    text ctx "(";
    write ctx paren.enc_desc;
    text ctx ")"

  | Var name
  | Uscore name ->
    text ctx name.desc

  | Macro macro ->
    text ctx "?";
    text ctx macro.macro_name.desc

  | Int value
  | Float value ->
    text ctx value.desc

  | Atom (`Unenclosed value) ->
    text ctx value.desc

  | Atom (`Enclosed value) ->
    text ctx "'";
    text ctx value.desc;
    text ctx "'"

  | String values ->
    List.iter values ~f:(fun value ->
        text ctx "\"";
        text ctx value.desc;
        text ctx "\"")

  | Tuple tuple ->
    text ctx "{";
    write_exp_list tuple.enc_desc;
    text ctx "}"

  | List list ->
    text ctx "[";
    write_exp_list list.list_head;
    Option.iter list.list_tail ~f:(fun tail ->
        text ctx " | ";
        write ctx tail);
    text ctx "]"

  | _ -> text ctx "?"

type formatted = {
  fmt_mod_name: Ast.t list;
  fmt_export : Ast.t list;
  fmt_import : Ast.t list;
  fmt_include : Ast.t list;
  fmt_inclib : Ast.t list;
  fmt_attrs : Ast.t list;
  fmt_decls : Ast.t list;
}

let restruct_decls decls =
  let open Ast in
  List.fold_left
    decls
    ~init:{ fmt_mod_name = [];
            fmt_export = [];
            fmt_import = [];
            fmt_include = [];
            fmt_inclib = [];
            fmt_attrs = [];
            fmt_decls = [] }
    ~f:(fun attrs decl ->
        match decl with
        | Module_attr attr ->
          { attrs with fmt_attrs = decl :: attrs.fmt_attrs }
        | Modname_attr attr ->
          { attrs with fmt_mod_name = decl :: attrs.fmt_mod_name }
        | Export_attr attr ->
          { attrs with fmt_export = decl :: attrs.fmt_export }
        | Import_attr attr ->
          { attrs with fmt_import = decl :: attrs.fmt_import }
        | Include_attr attr ->
          { attrs with fmt_include = decl :: attrs.fmt_include }
        | Inclib_attr attr ->
          { attrs with fmt_inclib = decl :: attrs.fmt_inclib }
        | _ -> { attrs with fmt_decls = decl :: attrs.fmt_decls })

let restruct node =
  let open Ast in
  match node with
  | Module m ->
    let fmt = restruct_decls m.module_decls in
    { fmt_mod_name = List.rev fmt.fmt_mod_name;
      fmt_export = List.rev fmt.fmt_export;
      fmt_import = List.rev fmt.fmt_import;
      fmt_include = List.rev fmt.fmt_include;
      fmt_inclib = List.rev fmt.fmt_inclib;
      fmt_attrs = List.rev fmt.fmt_attrs;
      fmt_decls = List.rev fmt.fmt_decls;
    }
  | _ -> failwith "must be module node"

let format node =
  let buf = Buffer.create 2000 in
  let ctx = Context.create buf in
  let fmt = restruct node in

  let iter nodes =
    List.iter nodes ~f:(write ctx)
  in

  let iterln nodes =
    iter nodes;
    if List.length nodes <> 0 then Context.newline ctx
  in

  iterln fmt.fmt_mod_name;
  iterln fmt.fmt_export;
  iterln fmt.fmt_import;
  iterln fmt.fmt_include;
  iterln fmt.fmt_inclib;
  iterln fmt.fmt_attrs;
  iterln fmt.fmt_decls;

  Op.write buf @@ Context.contents ctx;
  Buffer.contents buf

