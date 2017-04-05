%{

open Located

let binexp left op right =
  Ast.Binexp {
      binexp_left = left;
      binexp_op = op;
      binexp_right = right }

let paren open_ value close =
  Ast.Paren (Ast.enclose open_ value close)

%}

%token <Ast.text> UIDENT
%token <Ast.text> LIDENT
%token <Ast.text> USCORE           (* "_" *)
%token <Ast.text> CHAR
%token <Ast.text> STRING
%token <Ast.text> INT
%token <Ast.text> FLOAT
%token <Ast.text> MODULE_ATTR      (* "-module" *)
%token <Ast.text> EXPORT_ATTR      (* "-export" *)
%token <Ast.text> IMPORT_ATTR      (* "-import" *)
%token <Ast.text> INCLUDE_ATTR     (* "-include" *)
%token <Ast.text> INCLIB_ATTR      (* "-include_lib" *)
%token <Ast.text> SPEC_ATTR        (* "-spec" *)
%token <Ast.text> DEFINE_ATTR      (* "-define" *)
%token <Ast.token> LPAREN
%token <Ast.token> RPAREN
%token <Ast.token> LBRACK
%token <Ast.token> RBRACK
%token <Ast.token> LBRACE
%token <Ast.token> RBRACE
%token <Ast.token> COMMA
%token <Ast.token> DOT              (* "." *)
%token <Ast.token> DOT3             (* "..." *)
%token <Ast.token> COLON
%token <Ast.token> SEMI
%token <Ast.token> NSIGN            (* "#" *)
%token <Ast.token> AND              (* "and" *)
%token <Ast.token> OR               (* "or" *)
%token <Ast.token> NOT              (* "not" *)
%token <Ast.token> LAND             (* "band" *)
%token <Ast.token> LOR              (* "bor" *)
%token <Ast.token> LXOR             (* "bxor" *)
%token <Ast.token> LNOT             (* "bnot" *)
%token <Ast.token> LSHIFT           (* "bsl" *)
%token <Ast.token> RSHIFT           (* "bsr" *)
%token <Ast.token> EQ               (* "=" *)
%token <Ast.token> EP               (* "!" *)
%token <Ast.token> Q                (* "?" *)
%token <Ast.token> BAR              (* "|" *)
%token <Ast.token> DBAR             (* "||" *)
%token <Ast.token> EQQ              (* "==" *)
%token <Ast.token> NE               (* "/=" *)
%token <Ast.token> XEQ              (* "=:=" *)
%token <Ast.token> XNE              (* "=/=" *)
%token <Ast.token> LT               (* "<" *)
%token <Ast.token> LE               (* "=<" *)
%token <Ast.token> GT               (* ">" *)
%token <Ast.token> GE               (* ">=" *)
%token <Ast.token> PLUS             (* "+" *)
%token <Ast.token> MINUS            (* "-" *)
%token <Ast.token> MUL              (* "*" *)
%token <Ast.token> DIV              (* "/" *)
%token <Ast.token> QUO              (* "div" *)
%token <Ast.token> REM              (* "rem" *)
%token <Ast.token> LIST_ADD         (* "++" *)
%token <Ast.token> LIST_DIFF        (* "--" *)
%token <Ast.token> RARROW           (* "->" *)
%token <Ast.token> LARROW           (* "<-" *)
%token <Ast.token> LARROW2          (* "<=" *)
%token <Ast.token> DLT              (* ">>" *)
%token <Ast.token> DGT              (* "<<" *)
%token <Ast.token> AFTER            (* "after" *)
%token <Ast.token> BEGIN            (* "begin" *)
%token <Ast.token> CASE             (* "case" *)
%token <Ast.token> CATCH            (* "catch" *)
%token <Ast.token> END              (* "end" *)
%token <Ast.token> FUN              (* "fun" *)
%token <Ast.token> IF               (* "if" *)
%token <Ast.token> OF               (* "of" *)
%token <Ast.token> RECEIVE          (* "receive" *)
%token <Ast.token> TRY              (* "try" *)
%token <Ast.token> WHEN             (* "when" *)
%token EOF

(*
%nonassoc shift
%nonassoc CATCH
%right LIST_ADD LIST_DIFF
%left PLUS MINUS
%nonassoc SEMI
%nonassoc NSIGN
%nonassoc COLON
*)

%left BAR

%start <Ast.t option> prog

%%

prog:
  | module_ { $1 }

module_:
  | EOF { None }
  | module_decls EOF { Some $1 }

module_decls:
  | module_decl+
  { Ast.Module { module_decls = $1 } }

module_decl:
  | module_attr { $1 }
  | fun_decl { $1 }

module_attr:
  | modname_attr { $1 }
  | export_attr { $1 }
  | import_attr { $1 }
  | include_attr { $1 }
  | include_lib_attr { $1 }
  | define_attr { $1 }
  | spec_attr { $1 }
  | MINUS LIDENT LPAREN exps RPAREN DOT
  { Ast.Module_attr {
      module_attr_minus = $1;
      module_attr_tag = $2;
      module_attr_open = $3;
      module_attr_values = $4;
      module_attr_close = $5;
      module_attr_dot = $6 }
  }

modname_attr:
  | MODULE_ATTR LPAREN LIDENT RPAREN DOT
  { Ast.Modname_attr {
      modname_attr_tag = $1;
      modname_attr_open = $2;
      modname_attr_name = $3;
      modname_attr_close = $4;
      modname_attr_dot = $5;
    }
  }

export_attr:
  | EXPORT_ATTR LPAREN LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast.Export_attr {
      export_attr_tag = $1;
      export_attr_open = $2;
      export_attr_fun_open = $3;
      export_attr_funs = $4;
      export_attr_fun_close = $5;
      export_attr_close = $6;
      export_attr_dot = $7;
    }
  }

fun_sigs:
  | rev_fun_sigs { Seplist.rev $1 }

rev_fun_sigs:
  | fun_sig { Seplist.one $1 }
  | rev_fun_sigs COMMA fun_sig { Seplist.cons $3 ~sep:$2 $1 }

fun_sig:
  | LIDENT DIV INT
  { {
      Ast.fun_sig_name = $1;
      fun_sig_sep = $2;
      fun_sig_arity = $3;
    }
  }

import_attr:
  | IMPORT_ATTR LPAREN LIDENT COMMA LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast.Import_attr {
      import_attr_tag = $1;
      import_attr_open = $2;
      import_attr_module = $3;
      import_attr_comma = $4;
      import_attr_fun_open = $5;
      import_attr_funs = $6;
      import_attr_fun_close = $7;
      import_attr_close = $8;
      import_attr_dot = $9;
    }
  }

include_attr:
  | INCLUDE_ATTR LPAREN STRING RPAREN DOT
  { Ast.Include_attr {
      include_attr_tag = $1;
      include_attr_open = $2;
      include_attr_file = $3;
      include_attr_close = $4;
      include_attr_dot = $5;
    }
  }

include_lib_attr:
  | INCLIB_ATTR LPAREN STRING RPAREN DOT
  { Ast.Inclib_attr {
      inclib_attr_tag = $1;
      inclib_attr_open = $2;
      inclib_attr_file = $3;
      inclib_attr_close = $4;
      inclib_attr_dot = $5;
    }
  }

define_attr:
  | DEFINE_ATTR LPAREN exp COMMA exp RPAREN DOT
  { Ast.Def_attr {
      def_attr_tag = $1;
      def_attr_open = $2;
      def_attr_name = $3;
      def_attr_comma = $4;
      def_attr_value = $5;
      def_attr_close = $6;
      def_attr_dot = $7;
    }
  }

spec_attr:
  | SPEC_ATTR LIDENT spec_clauses DOT
  { Ast.(Spec_attr {
      spec_attr_tag = $1;
      spec_attr_mname = None;
      spec_attr_fname = $2;
      spec_attr_clauses = $3;
      spec_attr_dot = $4; })
  }
  | SPEC_ATTR LIDENT COLON LIDENT spec_clauses DOT
  { Ast.(Spec_attr {
      spec_attr_tag = $1;
      spec_attr_mname = Some ($2, $3);
      spec_attr_fname = $4;
      spec_attr_clauses = $5;
      spec_attr_dot = $6; })
  }

spec_clauses:
  | rev_spec_clauses { Seplist.rev $1 }

rev_spec_clauses:
  | spec_clause { Seplist.one $1 }
  | rev_spec_clauses SEMI spec_clause { Seplist.cons $3 ~sep:$2 $1 }

spec_clause:
  | LPAREN RPAREN RARROW spec_type
  { Ast.({
      spec_clause_open = $1;
      spec_clause_args = None;
      spec_clause_close = $2;
      spec_clause_arrow = $3;
      spec_clause_return = $4;
      spec_clause_guard = None; })
  }
  | LPAREN RPAREN RARROW spec_type WHEN guard
  { Ast.({
      spec_clause_open = $1;
      spec_clause_args = None;
      spec_clause_close = $2;
      spec_clause_arrow = $3;
      spec_clause_return = $4;
      spec_clause_guard = Some ($5, $6); })
  }
  | LPAREN spec_args RPAREN RARROW spec_type
  { Ast.({
      spec_clause_open = $1;
      spec_clause_args = Some $2;
      spec_clause_close = $3;
      spec_clause_arrow = $4;
      spec_clause_return = $5;
      spec_clause_guard = None; })
  }
  | LPAREN spec_args RPAREN RARROW spec_type WHEN guard
  { Ast.({
      spec_clause_open = $1;
      spec_clause_args = Some $2;
      spec_clause_close = $3;
      spec_clause_arrow = $4;
      spec_clause_return = $5;
      spec_clause_guard = Some ($6, $7); })
  }

spec_args:
  | rev_spec_args { Seplist.rev $1 }

rev_spec_args:
  | spec_arg { Seplist.one $1 }
  | rev_spec_args COMMA spec_arg { Seplist.cons $3 ~sep:$2 $1 }

spec_arg:
  | spec_type { $1 }

  (* TODO *)
spec_type:
  | spec_type BAR spec_type { Ast.Spec_type.Nil }
  | atom { Ast.Spec_type.Nil }
  | LIDENT LPAREN RPAREN { Ast.Spec_type.Nil }
  | LIDENT LPAREN spec_type_args RPAREN { Ast.Spec_type.Nil }
  | INT { Ast.Spec_type.Int $1 }
  | LBRACK RBRACK { Ast.Spec_type.Nil }
  | LBRACK spec_type RBRACK { Ast.Spec_type.Nil }
  | DLT DGT {Ast.Spec_type.Nil}
  | DLT USCORE COLON INT DGT {Ast.Spec_type.Nil}
  | DLT USCORE COLON USCORE MUL INT DGT {Ast.Spec_type.Nil}
  | DLT USCORE COLON INT COMMA USCORE COLON USCORE MUL INT DGT {Ast.Spec_type.Nil}
  | FUN LPAREN RPAREN { Ast.Spec_type.Nil }
  | FUN LPAREN spec_fun_body RPAREN { Ast.Spec_type.Nil }

spec_type_args:
  | rev_spec_type_args { Seplist.rev $1 }

rev_spec_type_args:
  | spec_type { Seplist.one $1 }
  | rev_spec_type_args COMMA spec_type { Seplist.cons $3 ~sep:$2 $1 }

spec_fun_body:
  | LPAREN RPAREN RARROW spec_type { () }
  | LPAREN DOT3 RPAREN RARROW spec_type { () }
  | LPAREN spec_args RPAREN RARROW spec_type { () }

fun_decl:
  | fun_clauses DOT
  { Ast.Fun_decl {
      fun_decl_body = $1;
      fun_decl_dot = $2 }
  }

fun_clauses:
  | rev_fun_clauses { Seplist.rev $1 }

rev_fun_clauses:
  | fun_clause { Seplist.one $1 }
  | rev_fun_clauses SEMI fun_clause { Seplist.cons $3 ~sep:$2 $1 }

fun_clause:
  | fun_clause_def { $1 }
  | LIDENT fun_clause_def
  { { $2 with Ast.fun_clause_name = Some $1 } }

fun_clause_def:
  | LPAREN patterns_opt RPAREN RARROW body
  { Ast.({
      fun_clause_name = None;
      fun_clause_open = $1;
      fun_clause_ptns = $2;
      fun_clause_close = $3;
      fun_clause_when = None;
      fun_clause_guard = None;
      fun_clause_arrow = $4;
      fun_clause_body = $5 })
  }
  | LPAREN patterns_opt RPAREN WHEN guard RARROW body
  { Ast.({
      fun_clause_name = None;
      fun_clause_open = $1;
      fun_clause_ptns = $2;
      fun_clause_close = $3;
      fun_clause_when = Some $4;
      fun_clause_guard = Some $5;
      fun_clause_arrow = $6;
      fun_clause_body = $7 })
  }

guard:
  | rev_guard { Seplist.rev $1 }

rev_guard:
  | exp { Seplist.one $1 }
  | rev_guard COMMA exp { Seplist.cons $3 ~sep:$2 $1 }

body:
  | exps { $1 }

exps:
  | rev_exps { Seplist.rev $1 }

rev_exps:
  | exp { Seplist.one $1 }
  | rev_exps COMMA exp { Seplist.cons $3 ~sep:$2 $1 }

exps_opt:
  | exps { $1 }
  | (* empty *) { Seplist.empty }

exp:
  | CATCH exp { Ast.Catch ($1, $2) }
  | match_exp { $1 }

match_exp:
  | pattern EQ match_exp
  { binexp $1 (locate $2 Ast.Op_eq) $3 }
  | send_exp { $1 }

send_exp:
  | compare_exp EP send_exp
  { binexp $1 (locate $2 Ast.Op_ep) $3 }
  | compare_exp { $1 }

compare_exp:
  | list_conc_exp compare_op list_conc_exp { binexp $1 $2 $3 }
  | list_conc_exp { $1 }

compare_op:
  | EQQ { locate $1 Ast.Op_eqq }
  | NE { locate $1 Ast.Op_ne }
  | XEQ { locate $1 Ast.Op_xeq }
  | XNE { locate $1 Ast.Op_xne }
  | GT { locate $1 Ast.Op_gt }
  | GE { locate $1 Ast.Op_ge }
  | LT { locate $1 Ast.Op_lt }
  | LE { locate $1 Ast.Op_le }

list_conc_exp:
  | shift_exp list_conc_op list_conc_exp { binexp $1 $2 $3 }
  | shift_exp { $1 }

list_conc_op:
  | LIST_ADD { locate $1 Ast.Op_list_add }
  | LIST_DIFF { locate $1 Ast.Op_list_diff }

shift_exp:
  | shift_exp shift_op mul_exp { binexp $1 $2 $3 }
  | mul_exp { $1 }

shift_op:
  | PLUS { locate $1 Ast.Op_add }
  | MINUS { locate $1 Ast.Op_sub }
  | LOR { locate $1 Ast.Op_lor }
  | LXOR { locate $1 Ast.Op_lxor }
  | LSHIFT { locate $1 Ast.Op_lshift }
  | RSHIFT { locate $1 Ast.Op_rshift }

mul_exp:
  | mul_exp mul_op prefix_exp { binexp $1 $2 $3 }
  | mul_exp AND prefix_exp
  { binexp $1 (locate $2 Ast.Op_and) $3 }
  | prefix_exp { $1 }

mul_op:
  | MUL { locate $1 Ast.Op_mul }
  | DIV { locate $1 Ast.Op_div }
  | QUO { locate $1 Ast.Op_quo }
  | REM { locate $1 Ast.Op_rem }
  | LAND { locate $1 Ast.Op_land }

prefix_exp:
  | prefix_op record_exp { Ast.Unexp ($1, $2) }
  | record_exp { $1 }

prefix_op:
  | PLUS { locate $1 Ast.Op_pos }
  | MINUS { locate $1 Ast.Op_neg }
  | NOT { locate $1 Ast.Op_not }
  | LNOT { locate $1 Ast.Op_lnot }

record_exp:
  | record_exp NSIGN LIDENT DOT LIDENT
  { Ast.Field {
      field_exp = Some $1;
      field_sharp = $2;
      field_rname = $3;
      field_sep = $4;
      field_fname = $5; }
  }
  | NSIGN LIDENT DOT LIDENT
  { Ast.Field {
      field_exp = None;
      field_sharp = $1;
      field_rname = $2;
      field_sep = $3;
      field_fname = $4; }
  }
  | record_exp NSIGN LIDENT LBRACE record_field_updates_opt RBRACE
  { Ast.Update {
      update_exp = Some $1;
      update_sharp = $2;
      update_name = $3;
      update_open = $4;
      update_assocs = $5;
      update_close = $6; }
  }
  | NSIGN LIDENT LBRACE record_field_updates_opt RBRACE
  { Ast.Update {
      update_exp = None;
      update_sharp = $1;
      update_name = $2;
      update_open = $3;
      update_assocs = $4;
      update_close = $5; }
  }
  | app_exp { $1 }

record_field_updates_opt:
  | record_field_updates { $1 }
  | (* empty *) { Seplist.empty }

record_field_updates:
  | rev_record_field_updates { Seplist.rev $1 }

rev_record_field_updates:
  | record_field_update { Seplist.one $1 }
  | rev_record_field_updates COMMA record_field_update
  { Seplist.cons $3 ~sep:$2 $1 }

record_field_update:
  | LIDENT EQ exp
  { { Ast.assoc_key = $1;
        assoc_val = $3;
        assoc_sep = $2; }
  }

app_exp:
  | primary_exp LPAREN exps_opt RPAREN
  { Ast.Call {
      call_fname = Ast.simple_fun_name $1;
      call_open = $2;
      call_args = $3;
      call_close = $4; }
  }
  | primary_exp COLON primary_exp LPAREN exps_opt RPAREN
  { let fname = {
      Ast.fun_name_mname = Some $1;
      fun_name_colon = Some $2;
      fun_name_fname = $3; }
    in
    Ast.Call {
      call_fname = fname;
      call_open = $4;
      call_args = $5;
      call_close = $6; }
  }
  | primary_exp { $1 }

primary_exp:
  | macro { $1 }
  | var { $1 }
  | atomic { $1 }
  | binary { $1 }
  | binary_compr { $1 }
  | tuple_skel { $1 }
  | list_skel { $1 }
  | list_compr { $1 }
  | block_exp { $1 }
  | if_exp { $1 }
  | case_exp { $1 }
  | receive_exp { $1 }
  | fun_exp { $1 }
  | try_exp { $1 }
  | LPAREN exp RPAREN { paren $1 $2 $3 }

macro:
  | Q UIDENT
  { Ast.Macro { macro_q = $1; macro_name = $2 } }
  | Q LIDENT
  { Ast.Macro { macro_q = $1; macro_name = $2 } }

var:
  | UIDENT { Ast.Var $1 }
  | USCORE { Ast.Uscore $1 }

atomic:
  | atom { $1 }
  | char { $1 }
  | string { $1 }
  | integer { $1 }
  | float { $1 }

atom:
  | LIDENT { (Ast.Atom $1) }

char:
  | CHAR { (Ast.Char $1) }

string:
  | STRING { (Ast.String $1) }

integer:
  | INT { (Ast.Int $1) }

float:
  | FLOAT { (Ast.Float $1) }

binary:
  | DGT DLT
  { Ast.(Binary (enclose $1 Seplist.empty $2)) }
  | DGT binary_elts DLT
  { Ast.(Binary (enclose $1 $2 $3)) }

binary_elts:
  | rev_binary_elts { Seplist.rev $1 }

rev_binary_elts:
  | binary_elt { Seplist.one $1 }
  | rev_binary_elts COMMA binary_elt { Seplist.cons $3 ~sep:$2 $1 }

binary_elt:
  | binary_value
  { Ast.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = None;
      bin_elt_size = None;
      bin_elt_slash = None;
      bin_elt_type = None; })
  }
  | binary_value COLON INT
  { Ast.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = Some $2;
      bin_elt_size = Some $3;
      bin_elt_slash = None;
      bin_elt_type = None; })
  }
  | binary_value COLON INT DIV binary_elt
  { Ast.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = Some $2;
      bin_elt_size = Some $3;
      bin_elt_slash = Some $4;
      bin_elt_type = Some $5; })
  }
  | binary_value DIV binary_elt
  { Ast.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = None;
      bin_elt_size = None;
      bin_elt_slash = Some $2;
      bin_elt_type = Some $3; })
  }

binary_value:
  | primary_exp { $1 }

binary_compr:
  | DGT binary DBAR binary_compr_quals DLT
  { Ast.Binary_compr {
      compr_open = $1;
      compr_exp = $2;
      compr_sep = $3;
      compr_quals = $4;
      compr_close = $5; }
  }

binary_compr_quals:
  | rev_binary_compr_quals { Seplist.rev $1 }

rev_binary_compr_quals:
  | binary_compr_qual { Seplist.one $1 }
  | rev_binary_compr_quals COMMA binary_compr_qual
  { Seplist.cons $3 ~sep:$2 $1 }

binary_compr_qual:
  | list_compr_gen { $1 }
  | binary_compr_gen { $1 }
  | binary_compr_filter { $1 }

binary_compr_gen:
  | pattern LARROW2 exp
  { Ast.Binary_compr_gen {
      bin_gen_ptn = $1;
      bin_gen_arrow = $2;
      bin_gen_exp = $3 }
  }

binary_compr_filter:
  | exp { $1 }

tuple_skel:
  | LBRACE exps_opt RBRACE
  { Ast.(Tuple (enclose $1 $2 $3)) }

list_skel:
  | LBRACK RBRACK
  { Ast.(List {
      list_open = $1;
      list_head = Seplist.empty;
      list_bar = None;
      list_tail = None;
      list_close = $2 })
  }
  | LBRACK exps RBRACK
  { Ast.(List {
      list_open = $1;
      list_head = $2;
      list_bar = None;
      list_tail = None;
      list_close = $3 })
  }
  | LBRACK exps BAR exp RBRACK
  { Ast.(List {
      list_open = $1;
      list_head = $2;
      list_bar = Some $3;
      list_tail = Some $4;
      list_close = $5 })
  }

list_compr:
  | LBRACK exp DBAR list_compr_quals RBRACK
  { Ast.List_compr {
      compr_open = $1;
      compr_exp = $2;
      compr_sep = $3;
      compr_quals = $4;
      compr_close = $5; }
  }

list_compr_quals:
  | rev_list_compr_quals { Seplist.rev $1 }

rev_list_compr_quals:
  | list_compr_qual { Seplist.one $1 }
  | rev_list_compr_quals COMMA list_compr_qual
  { Seplist.cons $3 ~sep:$2 $1 }

list_compr_qual:
  | list_compr_gen { $1 }
  | list_compr_filter { $1 }

list_compr_gen:
  | pattern LARROW exp
  { Ast.List_compr_gen {
      gen_ptn = $1;
      gen_arrow = $2;
      gen_exp = $3 }
  }

list_compr_filter:
  | exp { $1 }

block_exp:
  | BEGIN body END
  { Ast.(Block (enclose $1 $2 $3)) }

if_exp:
  | IF if_clauses END
  { (Ast.If {
      if_begin = $1;
      if_clauses = $2;
      if_end = $3 })
  }

if_clauses:
  | rev_if_clauses { Seplist.rev $1 }

rev_if_clauses:
  | if_clause { Seplist.one $1 }
  | rev_if_clauses SEMI if_clause { Seplist.cons $3 ~sep:$2 $1 }

if_clause:
  | guard RARROW body
  { { Ast.if_clause_guard = $1;
        if_clause_arrow = $2;
        if_clause_body = $3; }
  }

case_exp:
  | CASE exp OF cr_clauses END
  { (Ast.Case {
      case_begin = $1;
      case_exp = $2;
      case_of = $3;
      case_clauses = $4;
      case_end = $5; })
  }

cr_clauses:
  | rev_cr_clauses { Seplist.rev $1 }

rev_cr_clauses:
  | cr_clause { Seplist.one $1 }
  | rev_cr_clauses SEMI cr_clause { Seplist.cons $3 ~sep:$2 $1 }

cr_clause:
  | pattern RARROW body
  { { Ast.cr_clause_ptn = $1;
        Ast.cr_clause_when = None;
        Ast.cr_clause_guard = Seplist.empty;
        Ast.cr_clause_arrow = $2;
        Ast.cr_clause_body = $3; } }

  | pattern WHEN guard RARROW body
  { { Ast.cr_clause_ptn = $1;
        Ast.cr_clause_when = Some $2;
        Ast.cr_clause_guard = $3;
        Ast.cr_clause_arrow = $4;
        Ast.cr_clause_body = $5; } }

patterns:
  | rev_patterns { Seplist.rev $1 }

rev_patterns:
  | pattern { Seplist.one $1 }
  | rev_patterns COMMA pattern { Seplist.cons $3 ~sep:$2 $1 }

patterns_opt:
  | patterns { $1 }
  | (* empty *) { Seplist.empty }

(* TODO *)
pattern:
  | atomic { $1 }
  | var { $1 }
  | tuple_skel { $1 }
  | list_skel { $1 }
  | binary { $1 }
  (*| record_pattern { $1 }*)

receive_exp:
  | RECEIVE cr_clauses END
  { Ast.Recv {
      recv_begin = $1;
      recv_clauses = $2;
      recv_after = None;
      recv_end = $3; }
  }
  | RECEIVE AFTER exp RARROW body END
  { Ast.Recv {
      recv_begin = $1;
      recv_clauses = Seplist.empty;
      recv_after = Some {
        recv_after_begin = $2;
        recv_after_timer = $3;
        recv_after_arrow = $4;
        recv_after_body = $5;
      };
      recv_end = $6; }
  }
  | RECEIVE cr_clauses AFTER exp RARROW body END
  { Ast.Recv {
      recv_begin = $1;
      recv_clauses = $2;
      recv_after = Some {
        recv_after_begin = $3;
        recv_after_timer = $4;
        recv_after_arrow = $5;
        recv_after_body = $6;
      };
      recv_end = $7; }
  }

fun_exp:
  | FUN atom COLON atom_or_var DIV integer_or_var
  { Ast.Module_fun {
      module_fun_prefix = $1;
      module_fun_mname = Some $2;
      module_fun_colon = Some $3;
      module_fun_fname = $4;
      module_fun_slash = $5;
      module_fun_arity = $6; }
  }
  | FUN atom DIV integer_or_var
  { Ast.Module_fun {
      module_fun_prefix = $1;
      module_fun_mname = None;
      module_fun_colon = None;
      module_fun_fname = $2;
      module_fun_slash = $3;
      module_fun_arity = $4; }
  }
  | FUN fun_clauses END
  { Ast.Anon_fun {
      anon_fun_begin = $1;
      anon_fun_body = $2;
      anon_fun_end = $3; }
  }

atom_or_var:
  | atom { $1 }
  | var { $1 }

integer_or_var:
  | integer { $1 }
  | var { $1 }

try_exp:
  | TRY exps OF cr_clauses try_catch { Ast.Nop }
  | TRY exps try_catch { Ast.Nop }

try_catch:
  | CATCH try_clauses END
  { { Ast.try_catch_begin = Some $1;
        try_catch_clauses = Some $2;
        try_catch_after = None;
        try_catch_end = $3; }
  }
  | CATCH try_clauses AFTER exps END
  { { Ast.try_catch_begin = Some $1;
        try_catch_clauses = Some $2;
        try_catch_after = Some {
            try_catch_after_begin = $3;
            try_catch_after_exps = $4; };
        try_catch_end = $5; }
  }
  | AFTER exps END
  { { Ast.try_catch_begin = None;
        try_catch_clauses = None;
        try_catch_after = Some {
            try_catch_after_begin = $1;
            try_catch_after_exps = $2; };
        try_catch_end = $3; }
  }

try_clauses:
  | rev_try_clauses { Seplist.rev $1 }

rev_try_clauses:
  | try_clause { Seplist.one $1 }
  | rev_try_clauses SEMI try_clause
  { Seplist.cons $3 ~sep:$2 $1 }

try_clause:
  | primary_exp RARROW body
  { { Ast.try_clause_exn = None;
        try_clause_exp = $1;
        try_clause_guard = None;
        try_clause_body = $3; }
  }
  | primary_exp WHEN guard RARROW body
  { { Ast.try_clause_exn = None;
        try_clause_exp = $1;
        try_clause_guard = Some $3;
        try_clause_body = $5; }
  }
  | primary_exp COLON exp RARROW body
  { { Ast.try_clause_exn = Some ($1, $2);
        try_clause_exp = $3;
        try_clause_guard = None;
        try_clause_body = $5; }
  }
  | primary_exp COLON exp WHEN guard RARROW body
  { { Ast.try_clause_exn = Some ($1, $2);
        try_clause_exp = $3;
        try_clause_guard = Some $5;
        try_clause_body = $7; }
  }
