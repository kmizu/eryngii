open Core.Std
open Located

module Op = struct

  type t =
    | Nop
    | Nest
    | Unnest
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
    lines : string array;
    buf : Buffer.t;
    mutable ops : Op.t list;
    mutable indent : int list;
    mutable used_comments : Ast.text list;
    mutable count : int option;
    mutable state : [`Nop |
                     `Type_attr of int * int (* indent * enclose_count *)
                    ]; 
  }

  let create lines buf =
    { lines = Array.of_list lines;
      buf;
      ops = [];
      indent = [0];
      used_comments = [];
      count = None;
      state = `Nop;
    }

  let contents ctx =
    List.rev ctx.ops

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

  let update ctx state =
    ctx.state  <- state

  let enclose_type_attr ctx =
    match ctx.state with
    | `Type_attr (indent, enclose) ->
      update ctx (`Type_attr (indent, enclose+1))
    | _ -> ()

  let disclose_type_attr ctx =
    match ctx.state with
    | `Type_attr (indent, enclose) ->
      update ctx (`Type_attr (indent, enclose-1))
    | _ -> ()

  let add ctx op =
    ctx.ops <- op :: ctx.ops;
    Option.iter ctx.count ~f:(fun count ->
        let add = 
          match op with
          | Text s -> String.length s
          | Space n -> n
          | Newline -> 1
          | _ -> 0
        in
        ctx.count <- Some (count + add))

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

  let string ctx s =
    text ctx "\"";
    text ctx s;
    text ctx "\""

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

  let nest ?indent ctx =
    let indent = match indent with
      | Some n -> n
      | None -> 4 * List.length ctx.indent
    in
    ctx.indent <- indent :: ctx.indent

  let unnest ctx =
    ctx.indent <- List.tl_exn ctx.indent

  let cur_indent ctx =
    List.hd_exn ctx.indent

  let indent ctx =
    cur_indent ctx |> spaces ctx 

  let use_comment ctx com = 
    if List.existsi ctx.used_comments
        ~f:(fun _ used ->
            Location.(start_line_exn com = start_line_exn used)) then
      `Used
    else begin
      ctx.used_comments <- com :: ctx.used_comments;
      `Unused
    end

end

let format_comment node line =
  let line = String.substr_replace_all line
      ~pattern:"\t"
      ~with_:"    "
  in
  let line = String.lstrip line ~drop:(fun c -> c = '%') in
  let line = String.rstrip line in
  let prefix =
    match node with
    | Ast.Modname_attr _ -> "%%%"
    | Export_attr _
    | Export_type_attr _
    | Import_attr _
    | Include_attr _
    | Inclib_attr _
    | Define_attr _
    | Type_attr _
    | Opaque_attr _
    | Spec_attr _
    | Flow_attr _
    | Flow_macro_attr _
    | Fun_decl _ -> "%%"
    | _ -> "%"
  in
  if String.is_prefix line ~prefix:" " then
    prefix ^ line
  else
    prefix ^ " " ^ line

let write_comment ctx node =
  let open Context in
  let open Located in
  let open Location in
  let open Position in

  let pos = Ast.start_pos node in
  let all = Annot.all_annots () in
  let _, _, comments = Array.fold_right all
      ~init:(Array.length all - 1, true, [])
      ~f:(fun annot (i, cont, accu) ->
          let i' = i - 1 in
          if not cont then
            (i', false, accu)
          else if not (cont && i < pos.line) then
            (i', true, accu)
          else begin
            match Annot.comment i with
            | Some text ->
              begin match use_comment ctx text with
                | `Used ->
                  (i', true, accu)
                | `Unused ->
                  let s = format_comment node text.desc in
                  (i', true, s :: accu)
              end
            | None ->
              let line = String.strip @@ Array.get ctx.lines i in
              if String.is_empty line then
                (i', true, accu)
              else if not (String.is_prefix line ~prefix:"%") then
                (i', false, accu)
              else
                failwith "must not be executed"
          end)
  in
  List.iter comments ~f:(fun comment ->
      text ctx comment;
      newline ctx;
      indent ctx)

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

  let write_seplist ?(split=false) seplist sep =
    Seplist.iter seplist ~f:(fun sep_opt node ->
        if split then
          indent ctx;
        write ctx node;
        write_sep sep_opt sep;
        if split then
          newline ctx)
  in

  let write_exp_list ?split exp_list =
    write_seplist ?split exp_list ", "
  in

  let write_guard guard =
    Seplist.iter guard ~f:(fun sep exp_list ->
        write_exp_list exp_list;
        write_sep sep ", ")
  in

  let write_when_guard w guard =
    match (w, guard) with
    | Some w, Some guard ->
      text ctx "when ";
      write_guard guard;
      text ctx " "
    | _ -> ()
  in

  let write_fun_sig fsig =
    text ctx fsig.fun_sig_name.desc;
    text ctx "/";
    text ctx fsig.fun_sig_arity.desc
  in

  let write_fun_sigs fsigs ~indent =
    let len = Seplist.length fsigs in
    if len < 3 then
      Seplist.iter fsigs
        ~f:(fun sep fsig ->
            write_fun_sig fsig;
            write_sep sep ", ")
    else
      Seplist.iteri fsigs
        ~f:(fun i sep fsig ->
            if i > 0 then begin
              text ctx @@ String.make indent ' '
            end;
            write_fun_sig fsig;
            write_sep sep ",";
            if i+1 < len then
              newline ctx)
  in

  let write_pattern ptn =
    write ctx ptn
  in

  let write_patterns ptns =
    Seplist.iter ptns ~f:(fun sep ptn ->
        write_pattern ptn;
        write_sep sep ", ")
  in

  let write_cr_clause clause =
    write ctx clause.cr_clause_ptn;
    Option.iter clause.cr_clause_when
      ~f:(fun _ ->
          text ctx " when ";
          write_guard @@ Option.value_exn clause.cr_clause_guard);
    text ctx " ->";
    newline ctx;
    nest ctx;
    indent ctx;
    write_exp_list clause.cr_clause_body;
    unnest ctx;
  in

  let write_cr_clauses clauses =
    Seplist.iter clauses ~f:(fun sep clause ->
        indent ctx;
        write_cr_clause clause;
        write_sep sep ";\n")
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
        indent ctx;
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
    Option.iter name.fun_name_mname
      ~f:(fun mname ->
          write ctx mname;
          text ctx ":");
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

  let rec write_spec_type ty =
    let open Spec_type in

    begin match (ty, ctx.state) with
      | Union _, _ -> ()
      | _, `Type_attr (indent, 0) ->
        newline ctx;
        spaces ctx indent;
        text ctx "| "
      | _ -> ()
    end;

    match ty with
    | Atom (`Unenclosed name) ->
      text ctx name.desc
    | Atom (`Enclosed name) ->
      text ctx "'";
      text ctx name.desc;
      text ctx "'"
    | Int value ->
      text ctx value.desc
    | Nil _ ->
      text ctx "[]"
    | Tuple tuple ->
      enclose_type_attr ctx;
      text ctx "{";
      Seplist.opt_iter tuple.tuple_elts ~f:(fun sep arg ->
          write_spec_type arg;
          write_sep sep ", ");
      text ctx "}";
      disclose_type_attr ctx;
    | List ty ->
      enclose_type_attr ctx;
      text ctx "[";
      write_spec_type ty.enc_desc;
      text ctx "]";
      disclose_type_attr ctx;
    | Named named ->
      text ctx named.named_name.desc;
      text ctx "(";
      Seplist.opt_iter named.named_args ~f:(fun sep arg ->
          write_spec_type arg;
          write_sep sep ", ");
      text ctx ")"
    | Constraint constr ->
      text ctx constr.constr_name.desc;
      text ctx " :: ";
      write_spec_type constr.constr_type
    | Union union ->
      write_spec_type union.union_left;
      begin match ctx.state with
        | `Type_attr (_, 0) -> ()
        | _ -> text ctx " | "
      end;
      write_spec_type union.union_right
    | _ -> ()
  in

  let write_spec_args spec_args = 
    Seplist.opt_iter spec_args ~f:(fun sep arg ->
        write_spec_type arg;
        write_sep sep ", ")
  in

  let write_def_name name =
    text ctx name.def_name.desc;
    Option.iter name.def_args ~f:(fun args ->
        text ctx "(";
        Seplist.iter args.enc_desc ~f:(fun sep arg ->
            text ctx @@ Naming.uppercase arg.desc;
            write_sep sep ", ");
        text ctx ")")
  in

  write_comment ctx node;

  match node with
  | Module m -> iter m.module_decls

  | Modname_attr attr ->
    text ctx "-module(";
    text ctx attr.modname_attr_name.desc;
    text ctx ").";
    newline ctx

  | Behav_attr attr ->
    text ctx "-behaviour(";
    text ctx attr.behav_attr_name.desc;
    text ctx ").";
    newline ctx

  | Export_attr attr ->
    text ctx "-export([";
    write_fun_sigs attr.export_attr_funs ~indent:9;
    text ctx "]).";
    newline ctx

  | Export_type_attr attr ->
    text ctx "-export_type([";
    write_fun_sigs attr.export_attr_funs ~indent:14;
    text ctx "]).";
    newline ctx

  | Import_attr attr ->
    text ctx "-import(";
    text ctx attr.import_attr_module.desc;
    text ctx ", [";
    write_fun_sigs attr.import_attr_funs
      ~indent:(11 + String.length attr.import_attr_module.desc);
    text ctx "]).";
    newline ctx

  | Include_attr attr ->
    text ctx "-include(";
    string ctx attr.include_attr_file.desc;
    text ctx ").";
    newline ctx

  | Inclib_attr attr ->
    text ctx "-include_lib(";
    string ctx attr.inclib_attr_file.desc;
    text ctx ").";
    newline ctx

  | Define_attr attr ->
    text ctx "-define(";
    (*text ctx @@ Naming.uppercase attr.def_attr_name.desc;*)
    write_def_name attr.def_attr_name;
    text ctx ", ";
    write ctx attr.def_attr_value;
    text ctx ").";
    newline ctx

  | Spec_attr attr ->
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
          write_spec_args clause.spec_clause_args;
          text ctx ") -> ";
          write_spec_type clause.spec_clause_return;
          write_sep sep ";");
    dot_newline ctx

  | Type_attr attr ->
    start_count ctx;
    text ctx "-type ";
    text ctx attr.type_attr_name.desc;
    text ctx "(";
    write_spec_args attr.type_attr_args;
    text ctx ") :: ";
    let count = end_count ctx - 2 in
    begin match attr.type_attr_type with
      | Union union ->
        write_spec_type union.union_left;
        update ctx (`Type_attr (count, 0));
        write_spec_type union.union_right;
      | other ->
        write_spec_type other
    end;
    update ctx `Nop;
    dot_newline ctx

  | Record_attr attr ->
    text ctx "-record(";
    text ctx attr.rec_attr_name.desc;
    text ctx ", {";
    (*rec_attr_fields : Spec_type.field node_list option;*)
    Option.iter attr.rec_attr_fields
      ~f:(fun fields ->
          Seplist.iter fields ~f:(fun sep field ->
              text ctx field.field_name.desc;
              Option.iter field.field_init ~f:(fun ty ->
                  text ctx " = ";
                  write_spec_type ty);
              text ctx " :: ";
              write_spec_type field.field_type;
              Option.iter sep ~f:(fun _ -> text ctx ", ")));
    text ctx "})";
    dot_newline ctx

  | Flow_macro_attr attr ->
    text ctx "-";
    text ctx (match attr.flow_macro_attr_tag_type with
        | `Undef -> "undef"
        | `Ifdef -> "ifdef"
        | `Ifndef -> "ifndef");
    text ctx "(";
    text ctx attr.flow_macro_attr_macro.desc;
    text ctx ")";
    dot_newline ctx

  | Flow_attr attr ->
    text ctx "-";
    text ctx (match attr.flow_attr_tag_type with
        | `Else -> "else"
        | `Endif -> "endif");
    dot_newline ctx

  | Fun_decl decl ->
    nest ctx;
    write_fun_body decl.fun_decl_body;
    text ctx ".";
    newlines ctx;
    unnest ctx

  | If if_ ->
    text ctx "if ";
    Seplist.iter if_.if_clauses ~f:(fun sep clause ->
        write_guard clause.if_clause_guard;
        text ctx " -> ";
        write_exp_list clause.if_clause_body;
        Option.iter sep ~f:(fun _ -> text ctx ";\n"));
    text ctx " end"

  | Case case ->
    text ctx "case ";
    write ctx case.case_exp;
    text ctx " of";
    newline ctx;
    indent ctx;
    write_cr_clauses case.case_clauses;
    newline ctx;
    indent ctx;
    text ctx "end"

  | Try try_ ->
    text ctx "try";
    newline ctx;
    nest ctx;
    write_exp_list ~split:true try_.try_exps;
    Option.iter try_.try_of ~f:(fun _ -> text ctx " of ");
    unnest ctx;
    let catch = try_.try_catch in
    Option.iter catch.try_catch_clauses ~f:(fun clauses ->
        indent ctx;
        text ctx "catch";
        newline ctx;
        nest ctx;
        Seplist.iter clauses ~f:(fun sep clause ->
            indent ctx;
            Option.iter clause.try_clause_exn
              ~f:(fun (exn, _) ->
                  atom ctx exn;
                  text ctx ":");
            write ctx clause.try_clause_exp;
            Option.iter clause.try_clause_guard ~f:(fun (_when, guard) ->
                text ctx " when ";
                write_guard guard);
            text ctx " ->";
            newline ctx;
            nest ctx;
            write_exp_list ~split:true clause.try_clause_body;
            unnest ctx);
        unnest ctx);
    indent ctx;
    text ctx "end"

  | Call call ->
    write_fun_name call.call_fname;
    text ctx "(";
    write_exp_list call.call_args;
    text ctx ")"

  | Unexp (op, exp) ->
    write_op op.desc;
    space ctx;
    write ctx exp

  | Binexp exp ->
    write ctx exp.binexp_left;
    space ctx;
    write_op exp.binexp_op.desc;
    space ctx;
    write ctx exp.binexp_right

  | List_compr compr ->
    text ctx "[";
    write ctx compr.compr_exp;
    text ctx " || ";
    write_exp_list compr.compr_quals;
    text ctx "]"

  | List_compr_gen gen ->
    write ctx gen.gen_ptn;
    text ctx " -> ";
    write ctx gen.gen_exp

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
    List.iter values ~f:(fun value -> string ctx value.desc)

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

  | Anon_fun f ->
    text ctx "fun";
    write_fun_body f.anon_fun_body;
    text ctx " end"

  | _ -> failwith "not impl"

type formatted = {
  fmt_mod_name: Ast.t list;
  fmt_behav : Ast.t list;
  fmt_export : Ast.t list;
  fmt_export_type : Ast.t list;
  fmt_import : Ast.t list;
  fmt_inclib : Ast.t list;
  fmt_include : Ast.t list;
  fmt_define : Ast.t list;
  fmt_type : Ast.t list;
  fmt_record : Ast.t list;
  fmt_decls : Ast.t list;
}

let restruct_decls decls =
  let open Ast in
  List.fold_left
    decls
    ~init:{ fmt_mod_name = [];
            fmt_behav = [];
            fmt_export = [];
            fmt_export_type = [];
            fmt_import = [];
            fmt_inclib = [];
            fmt_include = [];
            fmt_define = [];
            fmt_type = [];
            fmt_record = [];
            fmt_decls = [] }
    ~f:(fun attrs decl ->
        match decl with
        | Modname_attr attr ->
          { attrs with fmt_mod_name = decl :: attrs.fmt_mod_name }
        | Behav_attr attr ->
          { attrs with fmt_behav = decl :: attrs.fmt_behav }
        | Export_attr attr ->
          { attrs with fmt_export = decl :: attrs.fmt_export }
        | Export_type_attr attr ->
          { attrs with fmt_export_type = decl :: attrs.fmt_export_type }
        | Import_attr attr ->
          { attrs with fmt_import = decl :: attrs.fmt_import }
        | Inclib_attr attr ->
          { attrs with fmt_inclib = decl :: attrs.fmt_inclib }
        | Include_attr attr ->
          { attrs with fmt_include = decl :: attrs.fmt_include }
        | Define_attr attr ->
          { attrs with fmt_define = decl :: attrs.fmt_define }
        | Type_attr attr ->
          { attrs with fmt_type = decl :: attrs.fmt_type }
        | Record_attr attr ->
          { attrs with fmt_record = decl :: attrs.fmt_record }
        | _ -> { attrs with fmt_decls = decl :: attrs.fmt_decls })

let restruct node =
  let open Ast in
  match node with
  | Module m ->
    let fmt = restruct_decls m.module_decls in
    { fmt_mod_name = List.rev fmt.fmt_mod_name;
      fmt_behav = List.rev fmt.fmt_behav;
      fmt_export = List.rev fmt.fmt_export;
      fmt_export_type = List.rev fmt.fmt_export_type;
      fmt_import = List.rev fmt.fmt_import;
      fmt_inclib = List.rev fmt.fmt_inclib;
      fmt_include = List.rev fmt.fmt_include;
      fmt_define = List.rev fmt.fmt_define;
      fmt_type = List.rev fmt.fmt_type;
      fmt_record = List.rev fmt.fmt_record;
      fmt_decls = List.rev fmt.fmt_decls;
    }
  | _ -> failwith "must be module node"

let format contents node =
  let buf = Buffer.create 2000 in
  let ctx = Context.create contents buf in
  let fmt = restruct node in

  let iter ?(newline=true) nodes =
    List.iter nodes ~f:(write ctx);
    if newline then
      Context.newline ctx
  in

  iter fmt.fmt_mod_name;
  iter fmt.fmt_behav;
  iter fmt.fmt_export;
  iter fmt.fmt_export_type;
  iter fmt.fmt_import;
  iter fmt.fmt_inclib;
  iter fmt.fmt_include;
  iter fmt.fmt_define;
  iter fmt.fmt_type;
  iter fmt.fmt_record;
  Context.newlines ctx;
  iter fmt.fmt_decls;

  Op.write buf @@ Context.contents ctx;
  String.strip @@ Buffer.contents buf

