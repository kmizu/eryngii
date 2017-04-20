open Core.Std
open Located

module Op = struct

  type t =
    | Nop
    | Text of string
    | Space of int
    | Newline

  let write buf ops =
    let open Buffer in
    ignore @@ List.fold_left
      ops
      ~init:Nop 
      ~f:(fun prev op ->
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
    mutable pos : Position.t;
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
      pos = Position.zero;
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
    ctx.pos <- begin match op with
      | Text s -> Position.add ctx.pos ~length:(String.length s)
      | Space n -> Position.add ctx.pos ~length:n
      | Newline -> Position.newline ctx.pos
      | _ -> ctx.pos
    end;
    Option.iter ctx.count ~f:(fun count ->
        let add = 
          match op with
          | Text s -> String.length s
          | Space n -> n
          | Newline -> 1
          | _ -> 0
        in
        ctx.count <- Some (count + add))

  let newline ?(ln=1) ctx =
    for i = 1 to ln do
      add ctx @@ Op.Newline
    done

  let text ctx s =
    add ctx @@ Op.Text s

  let textln ?(ln=1) ctx s =
    add ctx @@ Op.Text s;
    newline ctx ~ln

  let container ctx ~enclose ~f =
    let open_, close = enclose in
    text ctx open_;
    f ctx.pos;
    text ctx close

  let atom ctx = function
    | `Unenclosed name ->
      text ctx name.desc
    | `Enclosed name ->
      text ctx @@ "'" ^ name.desc ^ "'"

  let string ctx s =
    text ctx @@ "\"" ^ s ^ "\""

  let space ctx =
    add ctx @@ Op.Space 1

  let spaces ctx n =
    add ctx @@ Op.Space n

  let dot_newline ctx =
    textln ctx "."

  let cur_indent ctx =
    List.hd_exn ctx.indent

  let indent ctx =
    cur_indent ctx |> spaces ctx 

  let nest ?indent:size ctx =
    let size = (Option.value size ~default:4) + cur_indent ctx in
    ctx.indent <- size :: ctx.indent

  let unnest ctx =
    ctx.indent <- List.tl_exn ctx.indent

  let block ?indent:size ?enclose ?(openln=true) ?(closeln=false) ctx ~f =
    nest ctx ?indent:size;
    Option.iter enclose ~f:(fun (open_, _) -> text ctx open_);
    let pos = ctx.pos in
    if openln then begin
      newline ctx
    end;
    f pos;
    unnest ctx;

    begin match enclose with
      | None -> ()
      | Some (_, close) ->
        newline ctx;
        indent ctx;
        text ctx close
    end;
    if closeln then begin
      newline ctx
    end

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
    | Compile_attr _
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
      textln ctx comment;
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

  let write_sepln opt sep =
    match opt with
    | None -> ()
    | Some _ ->
      text ctx sep;
      newline ctx
  in

  let write_seplist ?(split=false) seplist sep =
    let len = Seplist.length seplist in
    Seplist.iteri seplist ~f:(fun i sep_opt node ->
        if split && i = 0 then begin
          indent ctx
        end;
        write ctx node;
        write_sep sep_opt sep;
        if split && i < len-1 then begin
          newline ctx;
          indent ctx
        end)
  in

  let write_atoms atoms ~indent =
    block ctx ~indent ~openln:false ~f:(fun _ ->
        Seplist.iter atoms ~f:(fun sep atom ->
            let name = Ast.text_of_atom atom in
            text ctx name.desc;
            write_sep sep ", "))
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
    text ctx @@ fsig.fun_sig_name.desc ^ "/" ^ fsig.fun_sig_arity.desc
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
    block ctx ~f:(fun _ -> write_exp_list clause.cr_clause_body ~split:true)
  in

  let write_cr_clauses clauses =
    Seplist.iter clauses ~f:(fun sep clause ->
        indent ctx;
        write_cr_clause clause;
        write_sepln sep ";")
  in

  let write_fun_clause clause =
    Option.iter clause.fun_clause_name ~f:(fun name -> text ctx name.desc);
    container ctx
      ~enclose:("(", ") ")
      ~f:(fun _ -> write_patterns clause.fun_clause_ptns);
    write_when_guard clause.fun_clause_when clause.fun_clause_guard;
    textln ctx "->";
    Seplist.iter clause.fun_clause_body ~f:(fun sep exp ->
        indent ctx;
        write ctx exp;
        write_sepln sep ",")
  in

  let write_fun_body body =
    Seplist.iter body
      ~f:(fun sep clause ->
          write_fun_clause clause;
          write_sepln sep ";")
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
      text ctx @@ "'" ^ name.desc ^ "'"
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
      text ctx @@ named.named_name.desc;
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
    textln ctx @@ "-module(" ^ attr.modname_attr_name.desc ^ ")."

  | Behav_attr attr ->
    textln ctx @@ "-behaviour(" ^ attr.behav_attr_name.desc ^ ")."

  | Compile_attr attr ->
    text ctx "-compile([";
    write_atoms attr.compile_attr_names ~indent:10;
    textln ctx "])."

  | Export_attr attr ->
    text ctx "-export([";
    write_fun_sigs attr.export_attr_funs ~indent:9;
    textln ctx "])."

  | Export_type_attr attr ->
    text ctx "-export_type([";
    write_fun_sigs attr.export_attr_funs ~indent:14;
    textln ctx "])."

  | Import_attr attr ->
    text ctx @@ "-import(" ^ attr.import_attr_module.desc ^ ", [";
    write_fun_sigs attr.import_attr_funs
      ~indent:(11 + String.length attr.import_attr_module.desc);
    textln ctx "])."

  | Include_attr attr ->
    container ctx
      ~enclose:("-include(", ").")
      ~f:(fun _ -> string ctx attr.include_attr_file.desc);
    newline ctx

  | Inclib_attr attr ->
    container ctx
      ~enclose:("-include_lib(", ").")
      ~f:(fun _ -> string ctx attr.inclib_attr_file.desc);
    newline ctx

  | Define_attr attr ->
    container ctx
      ~enclose:("-define(", ").")
      ~f:(fun _ ->
          (*text ctx @@ Naming.uppercase attr.def_attr_name.desc;*)
          write_def_name attr.def_attr_name;
          text ctx ", ";
          write ctx attr.def_attr_value);
    newline ctx

  | Spec_attr attr ->
    text ctx "-spec ";
    begin match attr.spec_attr_mname with
      | None -> ()
      | Some (mname, _) ->
        text ctx @@ mname.desc ^ ":"
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
    text ctx @@ "-type " ^ attr.type_attr_name.desc ^ "(";
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

  | Opaque_attr attr ->
    start_count ctx;
    text ctx @@ "-opaque " ^ attr.type_attr_name.desc ^ "(";
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

  | Opt_cbs_attr attr ->
    text ctx "-optional_callbacks([";
    write_fun_sigs attr.opt_attr_funs ~indent:21;
    textln ctx "])."

  | Record_attr attr ->
    text ctx @@ "-record(" ^ attr.rec_attr_name.desc ^ ", {";
    (*rec_attr_fields : Spec_type.field node_list option;*)
    Option.iter attr.rec_attr_fields
      ~f:(fun fields ->
          Seplist.iter fields ~f:(fun sep field ->
              text ctx field.field_name.desc;
              Option.iter field.field_init ~f:(fun ty ->
                  text ctx " = ";
                  write_spec_type ty);
              Option.iter field.field_type ~f:(fun ty ->
                  text ctx " :: ";
                  write_spec_type ty);
              Option.iter sep ~f:(fun _ -> text ctx ", ")));
    textln ctx "})."

  | Callback_attr attr ->
    text ctx @@ "-callback " ^ attr.cb_attr_name.desc;
    Seplist.iter attr.cb_attr_clauses
      ~f:(fun sep clause ->
          text ctx "(";
          write_spec_args clause.spec_clause_args;
          text ctx ") -> ";
          write_spec_type clause.spec_clause_return;
          write_sep sep ";");
    dot_newline ctx

  | Flow_macro_attr attr ->
    text ctx "-";
    text ctx (match attr.flow_macro_attr_tag_type with
        | `Undef -> "undef"
        | `Ifdef -> "ifdef"
        | `Ifndef -> "ifndef");
    textln ctx @@ "(" ^ attr.flow_macro_attr_macro.desc ^ ")."

  | Flow_attr attr ->
    text ctx "-";
    textln ctx (match attr.flow_attr_tag_type with
        | `Else -> "else."
        | `Endif -> "endif.")

  | Fun_decl decl ->
    block ctx ~f:(fun _ -> write_fun_body decl.fun_decl_body);
    textln ctx "."

  | Catch (_, exp) ->
    text ctx "catch ";
    write ctx exp

  | If if_ ->
    block ctx
      ~enclose:("if", "end")
      ~f:(fun _ ->
          Seplist.iter if_.if_clauses ~f:(fun sep clause ->
              write_guard clause.if_clause_guard;
              text ctx " -> ";
              write_exp_list clause.if_clause_body;
              write_sepln sep ";"))

  | Case case ->
    text ctx "case ";
    write ctx case.case_exp;
    text ctx " of";
    block ctx ~f:(fun _ -> write_cr_clauses case.case_clauses);
    newline ctx;
    indent ctx;
    text ctx "end"

  | Recv recv ->
    block ctx
      ~enclose:("receive", "end")
      ~f:(fun _ ->
          write_cr_clauses recv.recv_clauses;
          Option.iter recv.recv_after ~f:(fun after ->
              newline ctx;
              text ctx "after";
              block ctx ~f:(fun _ ->
                  indent ctx;
                  write ctx after.recv_after_timer;
                  text ctx " -> ";
                  write_exp_list after.recv_after_body)))

  | Try try_ ->
    text ctx "try";
    block ctx ~f:(fun _ -> write_exp_list ~split:true try_.try_exps);
    Option.iter try_.try_of ~f:(fun _ -> text ctx " of");
    newline ctx;
    let catch = try_.try_catch in
    Option.iter catch.try_catch_clauses ~f:(fun clauses ->
        indent ctx;
        text ctx "catch";
        block ctx ~f:(fun _ ->
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
                block ctx
                  ~f:(fun _ ->
                      write_exp_list clause.try_clause_body ~split:true;
                      write_sepln sep ";"))));
    Option.iter catch.try_catch_after ~f:(fun after ->
        newline ctx;
        indent ctx;
        text ctx "after";
        block ctx ~f:(fun _ ->
            write_exp_list after.try_catch_after_exps ~split:true));
    newline ctx;
    indent ctx;
    text ctx "end"

  | Call call ->
    write_fun_name call.call_fname;
    container ctx
      ~enclose:("(", ")")
      ~f:(fun _ -> write_exp_list call.call_args)

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
    container ctx
      ~enclose:("[", "]")
      ~f:(fun _ ->
          write ctx compr.compr_exp;
          text ctx " || ";
          write_exp_list compr.compr_quals)

  | List_compr_gen gen ->
    write ctx gen.gen_ptn;
    text ctx " -> ";
    write ctx gen.gen_exp

  | Block exps ->
    block ctx
      ~enclose:("begin", "end")
      ~f:(fun _ -> write_exp_list ~split:true exps.enc_desc)

  | Paren paren ->
    container ctx
      ~enclose:("(", ")")
      ~f:(fun _ -> write ctx paren.enc_desc)

  | Var name
  | Uscore name ->
    text ctx name.desc

  | Macro macro ->
    text ctx @@ "?" ^  macro.macro_name.desc

  | Int value
  | Float value ->
    text ctx value.desc

  | Atom (`Unenclosed value) ->
    text ctx value.desc

  | Atom (`Enclosed value) ->
    text ctx @@ "'" ^ value.desc ^ "'"

  | Char value ->
    container ctx
      ~enclose:("'", "'")
      ~f:(fun _ -> text ctx value.desc)

  | String values ->
    List.iter values ~f:(fun value -> string ctx value.desc)

  | Tuple tuple ->
    container ctx
      ~enclose:("{", "}")
      ~f:(fun _ -> write_exp_list tuple.enc_desc)

  | List list ->
    container ctx
      ~enclose:("[", "]")
      ~f:(fun _ ->
          write_exp_list list.list_head;
          Option.iter list.list_tail ~f:(fun tail ->
              text ctx " | ";
              write ctx tail))

  | Binary exps ->
    container ctx
      ~enclose:("<<", ">>")
      ~f:(fun _ -> write_exp_list exps.enc_desc)

  | Binary_elt elt ->
    write ctx elt.bin_elt_val;
    Option.iter elt.bin_elt_size
      ~f:(fun size -> text ctx @@ ":" ^ size.desc);
    Option.iter elt.bin_elt_type ~f:(fun ty ->
        text ctx "/";
        Seplist.iter ty ~f:(fun sep name ->
            text ctx name.desc;
            write_sep sep "-"))

  | Binary_compr compr ->
    text ctx "<<";
    write ctx compr.compr_exp;
    text ctx " || ";
    write_exp_list compr.compr_quals;
    text ctx ">>"

  | Binary_compr_gen gen ->
    write_pattern gen.bin_gen_ptn;
    text ctx " -> ";
    write ctx gen.bin_gen_exp

  | Module_fun f ->
    text ctx "fun ";
    Option.iter f.module_fun_mname
      ~f:(fun name ->
          write ctx name;
          text ctx ":");
    write ctx f.module_fun_fname;
    text ctx "/";
    write ctx f.module_fun_arity

  | Anon_fun f ->
    nest ctx ~indent:(ctx.pos.col - cur_indent ctx);
    text ctx "fun ";
    nest ctx ~indent:(ctx.pos.col - cur_indent ctx);
    Seplist.iteri f.anon_fun_body
      ~f:(fun i sep clause ->
          if i > 0 then begin
            indent ctx;
          end;
          nest ctx;
          write_fun_clause clause;
          write_sepln sep ";";
          unnest ctx);
    unnest ctx;
    newline ctx;
    indent ctx;
    text ctx "end";
    unnest ctx

  | Field field ->
    Option.iter field.field_exp ~f:(write ctx);
    text ctx @@ "#" ^ field.field_rname.desc ^ "." ^ field.field_fname.desc

  | Update exp ->
    Option.iter exp.update_exp ~f:(write ctx);
    text ctx @@ "#" ^ exp.update_name.desc ^ "{";
    Seplist.iter exp.update_assocs
      ~f:(fun sep assoc ->
          text ctx @@ assoc.assoc_key.desc ^ " = ";
          write ctx assoc.assoc_val;
          write_sep sep ", ");
    text ctx "}"

  | Map map ->
    Option.iter map.map_exp ~f:(write ctx);
    container ctx
      ~enclose:("#{", "}")
      ~f:(fun _ ->
          Seplist.opt_iter map.map_pairs ~f:(fun sep pair ->
              write ctx pair.map_pair_key;
              begin match pair.map_pair_op with
                | `Update _ -> text ctx " := "
                | `New _ -> text ctx " => "
              end;
              write ctx pair.map_pair_value;
              write_sep sep ", "))

  | Nop -> ()

type formatted = {
  fmt_mod_name: Ast.t list;
  fmt_behav : Ast.t list;
  fmt_compile : Ast.t list;
  fmt_export : Ast.t list;
  fmt_export_type : Ast.t list;
  fmt_import : Ast.t list;
  fmt_inclib : Ast.t list;
  fmt_include : Ast.t list;
  fmt_define : Ast.t list;
  fmt_type : Ast.t list;
  fmt_record : Ast.t list;
  fmt_callback : Ast.t list;
  fmt_opt_cbs: Ast.t list;
  fmt_decls : Ast.t list;
}

let restruct_decls decls =
  let open Ast in
  List.fold_left
    decls
    ~init:{ fmt_mod_name = [];
            fmt_behav = [];
            fmt_compile = [];
            fmt_export = [];
            fmt_export_type = [];
            fmt_import = [];
            fmt_inclib = [];
            fmt_include = [];
            fmt_define = [];
            fmt_type = [];
            fmt_record = [];
            fmt_callback = [];
            fmt_opt_cbs = [];
            fmt_decls = [] }
    ~f:(fun attrs decl ->
        match decl with
        | Modname_attr attr ->
          { attrs with fmt_mod_name = decl :: attrs.fmt_mod_name }
        | Behav_attr attr ->
          { attrs with fmt_behav = decl :: attrs.fmt_behav }
        | Compile_attr attr ->
          { attrs with fmt_compile = decl :: attrs.fmt_compile }
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
        | Type_attr attr
        | Opaque_attr attr ->
          { attrs with fmt_type = decl :: attrs.fmt_type }
        | Record_attr attr ->
          { attrs with fmt_record = decl :: attrs.fmt_record }
        | Callback_attr attr ->
          { attrs with fmt_callback = decl :: attrs.fmt_callback }
        | Opt_cbs_attr attr ->
          { attrs with fmt_opt_cbs = decl :: attrs.fmt_opt_cbs }
        | _ -> { attrs with fmt_decls = decl :: attrs.fmt_decls })

let restruct node =
  let open Ast in
  match node with
  | Module m ->
    let fmt = restruct_decls m.module_decls in
    { fmt_mod_name = List.rev fmt.fmt_mod_name;
      fmt_behav = List.rev fmt.fmt_behav;
      fmt_compile = List.rev fmt.fmt_compile;
      fmt_export = List.rev fmt.fmt_export;
      fmt_export_type = List.rev fmt.fmt_export_type;
      fmt_import = List.rev fmt.fmt_import;
      fmt_inclib = List.rev fmt.fmt_inclib;
      fmt_include = List.rev fmt.fmt_include;
      fmt_define = List.rev fmt.fmt_define;
      fmt_type = List.rev fmt.fmt_type;
      fmt_record = List.rev fmt.fmt_record;
      fmt_callback = List.rev fmt.fmt_callback;
      fmt_opt_cbs = List.rev fmt.fmt_opt_cbs;
      fmt_decls = List.rev fmt.fmt_decls;
    }
  | _ -> failwith "must be module node"

let format contents node =
  let buf = Buffer.create 2000 in
  let ctx = Context.create contents buf in
  let fmt = restruct node in

  let iter ?(newline=true) nodes =
    List.iter nodes ~f:(write ctx);
    if List.length nodes > 0 && newline then
      Context.newline ctx
  in

  iter fmt.fmt_mod_name;
  iter fmt.fmt_behav;
  iter fmt.fmt_compile;
  iter fmt.fmt_export;
  iter fmt.fmt_export_type;
  iter fmt.fmt_import;
  iter fmt.fmt_inclib;
  iter fmt.fmt_include;
  iter fmt.fmt_define;
  iter fmt.fmt_type;
  iter fmt.fmt_record;
  iter fmt.fmt_callback;
  iter fmt.fmt_opt_cbs;
  Context.newline ctx ~ln:2;
  iter fmt.fmt_decls ~newline:false;

  Op.write buf @@ Context.contents ctx;
  String.strip @@ Buffer.contents buf

