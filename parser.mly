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
%token <Ast.text> ATOM
%token <Ast.text> STRING
%token <Ast.text> INT
%token <Ast.text> FLOAT
%token <Ast.text> BEHAV_ATTR       (* "-behaviour" *)
%token <Ast.text> DEFINE_ATTR      (* "-define" *)
%token <Ast.text> EXPORT_ATTR      (* "-export" *)
%token <Ast.text> EXPORT_TYPE_ATTR (* "-export_type" *)
%token <Ast.text> IMPORT_ATTR      (* "-import" *)
%token <Ast.text> INCLUDE_ATTR     (* "-include" *)
%token <Ast.text> INCLIB_ATTR      (* "-include_lib" *)
%token <Ast.text> MODULE_ATTR      (* "-module" *)
%token <Ast.text> OPAQUE_ATTR      (* "-opaque" *)
%token <Ast.text> SPEC_ATTR        (* "-spec" *)
%token <Ast.text> TYPE_ATTR        (* "-type" *)
%token <Ast.text> RECORD_ATTR      (* "-record" *)
%token <Ast.text> UNDEF_ATTR       (* "-undef" *)
%token <Ast.text> IFDEF_ATTR       (* "-ifdef" *)
%token <Ast.text> IFNDEF_ATTR      (* "-ifndef" *)
%token <Ast.text> ELSE_ATTR        (* "-else" *)
%token <Ast.text> ENDIF_ATTR       (* "-endif" *)
%token <Ast.token> LPAREN
%token <Ast.token> RPAREN
%token <Ast.token> LBRACK
%token <Ast.token> RBRACK
%token <Ast.token> LBRACE
%token <Ast.token> RBRACE
%token <Ast.token> COMMA
%token <Ast.token> DOT              (* "." *)
%token <Ast.token> DOT2             (* ".." *)
%token <Ast.token> DOT3             (* "..." *)
%token <Ast.token> COLON
%token <Ast.token> COLON2
%token <Ast.token> SEMI
%token <Ast.token> NSIGN            (* "#" *)
%token <Ast.token> AND              (* "and" *)
%token <Ast.token> ANDALSO          (* "andalso" *)
%token <Ast.token> OR               (* "or" *)
%token <Ast.token> ORELSE           (* "orelse" *)
%token <Ast.token> NOT              (* "not" *)
%token <Ast.token> LAND             (* "band" *)
%token <Ast.token> LOR              (* "bor" *)
%token <Ast.token> LXOR             (* "bxor" *)
%token <Ast.token> LNOT             (* "bnot" *)
%token <Ast.token> LSHIFT           (* "bsl" *)
%token <Ast.token> RSHIFT           (* "bsr" *)
%token <Ast.token> EQ               (* "=" *)
%token <Ast.token> BANG             (* "!" *)
%token <Ast.token> Q                (* "?" *)
%token <Ast.token> BAR              (* "|" *)
%token <Ast.token> DBAR             (* "||" *)
%token <Ast.token> EQQ              (* "==" *)
%token <Ast.token> CEQ              (* ":=" *)
%token <Ast.token> NE               (* "/=" *)
%token <Ast.token> XEQ              (* "=:=" *)
%token <Ast.token> XNE              (* "=/=" *)
%token <Ast.token> LT               (* "<" *)
%token <Ast.token> LE               (* "=<" *)
%token <Ast.token> GT               (* ">" *)
%token <Ast.token> GE               (* ">=" *)
%token <Ast.token> PLUS             (* "+" *)
%token <Ast.token> PLUS2            (* "++" *)
%token <Ast.token> MINUS            (* "-" *)
%token <Ast.token> MINUS2           (* "--" *)
%token <Ast.token> MUL              (* "*" *)
%token <Ast.token> DIV              (* "/" *)
%token <Ast.token> QUO              (* "div" *)
%token <Ast.token> REM              (* "rem" *)
%token <Ast.token> RARROW           (* "->" *)
%token <Ast.token> RARROW2          (* "=>" *)
%token <Ast.token> LARROW           (* "<-" *)
%token <Ast.token> LARROW2          (* "<=" *)
%token <Ast.token> DLT              (* ">>" *)
%token <Ast.token> DGT              (* "<<" *)
%token <Ast.token> AFTER            (* "after" *)
%token <Ast.token> BEGIN            (* "begin" *)
%token <Ast.token> CASE             (* "case" *)
%token <Ast.token> CATCH            (* "catch" *)
%token <Ast.token> COND             (* "cond" *)
%token <Ast.token> END              (* "end" *)
%token <Ast.token> FUN              (* "fun" *)
%token <Ast.token> IF               (* "if" *)
%token <Ast.token> LET              (* "let" *)
%token <Ast.token> OF               (* "of" *)
%token <Ast.token> RECEIVE          (* "receive" *)
%token <Ast.token> TRY              (* "try" *)
%token <Ast.token> WHEN             (* "when" *)
%token <Ast.token> EOF

%nonassoc CATCH
%right EQ BANG
%left ORELSE
%left ANDALSO
%right PLUS2 MINUS2
%left PLUS MINUS
%nonassoc SEMI
%nonassoc NSIGN
%nonassoc COLON

%right BAR
%nonassoc COLON2

%start <Ast.t> prog

%%

prog:
  | module_ { $1 }

module_:
  | EOF
  { Ast.Module { module_decls = []; module_eof = $1 } }
  | module_decl+ EOF
  { Ast.Module { module_decls = $1; module_eof = $2 } }

module_decl:
  | module_attr { $1 }
  | fun_decl { $1 }

module_attr:
  | modname_attr { $1 }
  | export_attr { $1 }
  | export_type_attr { $1 }
  | import_attr { $1 }
  | include_attr { $1 }
  | include_lib_attr { $1 }
  | define_attr { $1 }
  | spec_attr { $1 }
  | type_attr { $1 }
  | opaque_attr { $1 }
  | behav_attr { $1 }
  | record_attr { $1 }
  | flow_macro_attr { $1 }

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

export_type_attr:
  | EXPORT_TYPE_ATTR LPAREN LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast.Export_type_attr {
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
  | DEFINE_ATTR LPAREN define_name COMMA exp RPAREN DOT
  { Ast.Define_attr {
      def_attr_tag = $1;
      def_attr_open = $2;
      def_attr_name = $3;
      def_attr_comma = $4;
      def_attr_value = $5;
      def_attr_close = $6;
      def_attr_dot = $7;
    }
  }

define_name:
  | macro_name { { Ast.def_name = $1; def_args = None } }
  | macro_name LPAREN define_args RPAREN
  { { Ast.def_name = $1;
      def_args = Some (Ast.enclose $2 $3 $4);
    }
  }

define_args:
  | rev_define_args { Seplist.rev $1 }

rev_define_args:
  | define_arg { Seplist.one $1 }
  | rev_define_args SEMI define_arg { Seplist.cons $3 ~sep:$2 $1 }

define_arg:
  | UIDENT { $1 }
  | ATOM { $1 }

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

spec_type:
  | raw_atom { Ast.Spec_type.Atom $1 }
  | spec_type_constraint { $1 }
  | spec_type_named { $1 }
  | spec_type_list { $1 }
  | spec_type_tuple { $1 }
  | spec_type_fun { $1 }
  | spec_type_bitstr { $1 }
  | spec_type_map { $1 }
  | spec_type_record { $1 }
  | spec_type_union { $1 }
  | LPAREN spec_type RPAREN
  { Ast.(Spec_type.Paren (enclose $1 $2 $3)) }
  | INT { Ast.Spec_type.Int $1 }
  | INT DOT2 INT
  { Ast.Spec_type.Range {
      range_start = $1;
      range_dot = $2;
      range_end = $3;
    }
  }
  | LBRACK RBRACK
  { Ast.Spec_type.Nil ($1, $2) }

spec_type_constraint:
  | UIDENT COLON2 spec_type
  { Ast.Spec_type.Constraint {
      constr_name = $1;
      constr_colon = $2;
      constr_type = $3;
    }
  }

spec_type_named:
  | LIDENT LPAREN RPAREN
  { Ast.Spec_type.Named {
      named_module = None;
      named_colon = None;
      named_name = $1;
      named_open = $2;
      named_args = None;
      named_close = $3;
    }
  }
  | LIDENT LPAREN spec_type_args RPAREN
  { Ast.Spec_type.Named {
      named_module = None;
      named_colon = None;
      named_name = $1;
      named_open = $2;
      named_args = Some $3;
      named_close = $4;
    }
  }
  | LIDENT COLON LIDENT LPAREN RPAREN
  { Ast.Spec_type.Named {
      named_module = Some $1;
      named_colon = Some $2;
      named_name = $3;
      named_open = $4;
      named_args = None;
      named_close = $5;
    }
  }
  | LIDENT COLON LIDENT LPAREN spec_type_args RPAREN
  { Ast.Spec_type.Named {
      named_module = Some $1;
      named_colon = Some $2;
      named_name = $3;
      named_open = $4;
      named_args = Some $5;
      named_close = $6;
    }
  }

spec_type_args:
  | rev_spec_type_args { Seplist.rev $1 }

rev_spec_type_args:
  | spec_type { Seplist.one $1 }
  | rev_spec_type_args COMMA spec_type { Seplist.cons $3 ~sep:$2 $1 }

spec_type_list:
  | LBRACK spec_type RBRACK
  { Ast.(Spec_type.List (enclose $1 $2 $3)) }

  spec_type_tuple:
  | LBRACE RBRACE
  { Ast.Spec_type.Tuple {
      tuple_open = $1;
      tuple_elts = None;
      tuple_close = $2;
    }
  }
  | LBRACE spec_type_args RBRACE
  { Ast.Spec_type.Tuple {
      tuple_open = $1;
      tuple_elts = Some $2;
      tuple_close = $3;
    }
  }

spec_type_fun:
  | FUN LPAREN RPAREN
  { Ast.Spec_type.Fun {
      fun_tag = $1;
      fun_open = $2;
      fun_body = None;
      fun_close = $3;
    }
  }
  | FUN LPAREN spec_fun_body RPAREN
  { Ast.Spec_type.Fun {
      fun_tag = $1;
      fun_open = $2;
      fun_body = Some $3;
      fun_close = $4;
    }
  }

spec_type_bitstr:
  | DLT DGT
  { Ast.Spec_type.Bits {
      bits_open = $1;
      bits_start_uscore = None;
      bits_start_colon = None;
      bits_start_bits = None;
      bits_comma = None;
      bits_cont_uscore1 = None;
      bits_cont_colon = None;
      bits_cont_uscore2 = None;
      bits_cont_mul = None;
      bits_cont_bits = None;
      bits_close = $2;
    }
  }
  | DLT USCORE COLON INT DGT
  { Ast.Spec_type.Bits {
      bits_open = $1;
      bits_start_uscore = Some $2;
      bits_start_colon = Some $3;
      bits_start_bits = Some $4;
      bits_comma = None;
      bits_cont_uscore1 = None;
      bits_cont_colon = None;
      bits_cont_uscore2 = None;
      bits_cont_mul = None;
      bits_cont_bits = None;
      bits_close = $5;
    }
  }
  | DLT USCORE COLON USCORE MUL INT DGT
  { Ast.Spec_type.Bits {
      bits_open = $1;
      bits_start_uscore = None;
      bits_start_colon = None;
      bits_start_bits = None;
      bits_comma = None;
      bits_cont_uscore1 = Some $2;
      bits_cont_colon = Some $3;
      bits_cont_uscore2 = Some $4;
      bits_cont_mul = Some $5;
      bits_cont_bits = Some $6;
      bits_close = $5;
    }
  }
  | DLT USCORE COLON INT COMMA USCORE COLON USCORE MUL INT DGT
  { Ast.Spec_type.Bits {
      bits_open = $1;
      bits_start_uscore = Some $2;
      bits_start_colon = Some $3;
      bits_start_bits = Some $4;
      bits_comma = Some $5;
      bits_cont_uscore1 = Some $6;
      bits_cont_colon = Some $7;
      bits_cont_uscore2 = Some $8;
      bits_cont_mul = Some $9;
      bits_cont_bits = Some $10;
      bits_close = $5;
    }
  }

spec_fun_body:
  | LPAREN RPAREN RARROW spec_type
  { { Ast.Spec_type.fun_body_open = $1;
      fun_body_args = `None;
      fun_body_close = $2;
      fun_body_arrow = $3;
      fun_body_type = $4;
    }
  }
  | LPAREN DOT3 RPAREN RARROW spec_type
  { { Ast.Spec_type.fun_body_open = $1;
      fun_body_args = `Dot $2;
      fun_body_close = $3;
      fun_body_arrow = $4;
      fun_body_type = $5;
    }
  }
  | LPAREN spec_args RPAREN RARROW spec_type
  { { Ast.Spec_type.fun_body_open = $1;
      fun_body_args = `Types $2;
      fun_body_close = $3;
      fun_body_arrow = $4;
      fun_body_type = $5;
    }
  }

spec_type_map:
  | NSIGN LBRACE RBRACE
  { Ast.Spec_type.Map { map_nsign = $1;
      map_open = $2;
      map_pairs = None;
      map_close = $3;
    }
  }
  | NSIGN LBRACE spec_pairs RBRACE
  { Ast.Spec_type.Map {
      map_nsign = $1;
      map_open = $2;
      map_pairs = Some $3;
      map_close = $4;
    }
  }

spec_pairs:
  | rev_spec_pairs { Seplist.rev $1 }

rev_spec_pairs:
  | spec_pair { Seplist.one $1 }
  | rev_spec_pairs COMMA spec_pair { Seplist.cons $3 ~sep:$2 $1 }

spec_pair:
  | spec_type CEQ spec_type
  { { Ast.Spec_type.pair_left = $1;
      pair_op = `Mandatory $2;
      pair_right = $3;
    }
  }
  | spec_type RARROW2 spec_type
  { { Ast.Spec_type.pair_left = $1;
      pair_op = `Optional $2;
      pair_right = $3;
    }
  }

spec_type_record:
  | NSIGN LIDENT LBRACE RBRACE
  { Ast.Spec_type.Record {
      rec_nsign = $1;
      rec_name = $2;
      rec_open = $3;
      rec_fields = None;
      rec_close = $4;
    }
  }
  | NSIGN LIDENT LBRACE type_fields RBRACE
  { Ast.Spec_type.Record {
      rec_nsign = $1;
      rec_name = $2;
      rec_open = $3;
      rec_fields = Some $4;
      rec_close = $5;
    }
  }

spec_type_union:
  | spec_type BAR spec_type
  { Ast.Spec_type.Union { union_left = $1;
      union_op = $2;
      union_right = $3;
    }
  }

type_attr:
  | TYPE_ATTR LIDENT LPAREN RPAREN COLON2 spec_type DOT
  { Ast.Type_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = None;
      type_attr_close = $4;
      type_attr_colon = $5;
      type_attr_type = $6;
      type_attr_dot = $7;
    }
  }
  | TYPE_ATTR LIDENT LPAREN spec_args RPAREN COLON2 spec_type DOT
  { Ast.Type_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = Some $4;
      type_attr_close = $5;
      type_attr_colon = $6;
      type_attr_type = $7;
      type_attr_dot = $8;
    }
  }

opaque_attr:
  | OPAQUE_ATTR LIDENT LPAREN RPAREN COLON2 spec_type DOT
  { Ast.Opaque_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = None;
      type_attr_close = $4;
      type_attr_colon = $5;
      type_attr_type = $6;
      type_attr_dot = $7;
    }
  }
  | OPAQUE_ATTR LIDENT LPAREN spec_args RPAREN COLON2 spec_type DOT
  { Ast.Opaque_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = Some $4;
      type_attr_close = $5;
      type_attr_colon = $6;
      type_attr_type = $7;
      type_attr_dot = $8;
    }
  }

behav_attr:
  | BEHAV_ATTR LPAREN LIDENT RPAREN DOT
  { Ast.Behav_attr {
      behav_attr_tag = $1;
      behav_attr_open = $2;
      behav_attr_name = $3;
      behav_attr_close = $4;
      behav_attr_dot = $5;
    }
  }

record_attr:
  | RECORD_ATTR LPAREN LIDENT COMMA LBRACE RBRACE RPAREN DOT
  { Ast.Record_attr {
      rec_attr_tag = $1;
      rec_attr_open = $2;
      rec_attr_name = $3;
      rec_attr_comma = $4;
      rec_attr_rec_open = $5;
      rec_attr_fields = None;
      rec_attr_rec_close = $6;
      rec_attr_close = $7;
      rec_attr_dot = $8;
    }
  }
  | RECORD_ATTR LPAREN LIDENT COMMA LBRACE type_fields RBRACE RPAREN DOT
  { Ast.Record_attr {
      rec_attr_tag = $1;
      rec_attr_open = $2;
      rec_attr_name = $3;
      rec_attr_comma = $4;
      rec_attr_rec_open = $5;
      rec_attr_fields = Some $6;
      rec_attr_rec_close = $7;
      rec_attr_close = $8;
      rec_attr_dot = $9;
    }
  }

type_fields:
  | rev_type_fields { Seplist.rev $1 }

rev_type_fields:
  | type_field { Seplist.one $1 }
  | rev_type_fields COMMA type_field { Seplist.cons $3 ~sep:$2 $1 }

type_field:
  | LIDENT COLON2 spec_type
  { { Ast.Spec_type.field_name = $1;
        field_eq = None;
        field_init = None;
        field_colon = $2;
        field_type = $3;
    }
  }
  | LIDENT EQ spec_type COLON2 spec_type
  { { Ast.Spec_type.field_name = $1;
        field_eq = Some $2;
        field_init = Some $3;
        field_colon = $4;
        field_type = $5;
    }
  }

flow_macro_attr:
  | UNDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast.Flow_macro_attr {
      flow_macro_attr_tag_type = `Undef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | IFDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast.Flow_macro_attr {
      flow_macro_attr_tag_type = `Ifdef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | IFNDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast.Flow_macro_attr {
      flow_macro_attr_tag_type = `Ifndef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | ELSE_ATTR DOT
  { Ast.Flow_attr {
      flow_attr_tag_type = `Else;
      flow_attr_tag = $1;
      flow_attr_dot = $2;
    }
  }
  | ENDIF_ATTR DOT
  { Ast.Flow_attr {
      flow_attr_tag_type = `Endif;
      flow_attr_tag = $1;
      flow_attr_dot = $2;
    }
  }

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
  | guard_clauses { Seplist.one $1 }
  | rev_guard SEMI guard_clauses { Seplist.cons $3 ~sep:$2 $1 }

guard_clauses:
  | rev_guard_clauses { Seplist.rev $1 }

rev_guard_clauses:
  | exp { Seplist.one $1 }
  | rev_guard_clauses COMMA exp { Seplist.cons $3 ~sep:$2 $1 }

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
  | match_exp EQ match_exp
  { binexp $1 (locate $2 Ast.Op_eq) $3 }
  | send_exp { $1 }

send_exp:
  | send_exp BANG send_exp
  { binexp $1 (locate $2 Ast.Op_ep) $3 }
  | or_cond_exp { $1 }

or_cond_exp:
  | or_cond_exp ORELSE or_cond_exp
  { binexp $1 (locate $2 Ast.Op_orelse) $3 }
  | and_cond_exp { $1 }

and_cond_exp:
  | and_cond_exp ANDALSO and_cond_exp
  { binexp $1 (locate $2 Ast.Op_andalso) $3 }
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
  | PLUS2 { locate $1 Ast.Op_list_add }
  | MINUS2 { locate $1 Ast.Op_list_diff }

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
  | prefix_exp { $1 }

mul_op:
  | MUL { locate $1 Ast.Op_mul }
  | DIV { locate $1 Ast.Op_div }
  | QUO { locate $1 Ast.Op_quo }
  | REM { locate $1 Ast.Op_rem }
  | AND { locate $1 Ast.Op_and }
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
  | map_exp { $1 }

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

map_exp:
  | NSIGN LBRACE RBRACE
  { Ast.Map {
      map_exp = None;
      map_nsign = $1;
      map_open = $2;
      map_pairs = None;
      map_close = $3;
    }
  }
  | record_exp NSIGN LBRACE RBRACE
  { Ast.Map {
      map_exp = Some $1;
      map_nsign = $2;
      map_open = $3;
      map_pairs = None;
      map_close = $4;
    }
  }
  | NSIGN LBRACE map_pairs RBRACE
  { Ast.Map {
      map_exp = None;
      map_nsign = $1;
      map_open = $2;
      map_pairs = Some $3;
      map_close = $4;
    }
  }
  | record_exp NSIGN LBRACE map_pairs RBRACE
  { Ast.Map {
      map_exp = Some $1;
      map_nsign = $2;
      map_open = $3;
      map_pairs = Some $4;
      map_close = $5;
    }
  }
  | app_exp { $1 }

map_pairs:
  | rev_map_pairs { $1 }

rev_map_pairs:
  | map_pair { Seplist.one $1 }
  | rev_map_pairs COMMA map_pair { Seplist.cons $3 ~sep:$2 $1 }

map_pair:
  | exp CEQ exp
  { { Ast.map_pair_key = $1;
        map_pair_op = `Update $2;
        map_pair_value = $3;
    }
  }
  | exp RARROW2 exp
  { { Ast.map_pair_key = $1;
        map_pair_op = `New $2;
        map_pair_value = $3;
    }
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
  | Q macro_name
  { Ast.Macro { macro_q = $1; macro_name = $2 } }

macro_name:
  | UIDENT { $1 }
  | LIDENT { $1 }
  | ATOM { $1 }

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
  | raw_atom { Ast.Atom $1 }

raw_atom:
  | LIDENT { `Unenclosed $1 }
  | ATOM { `Enclosed $1 }

char:
  | CHAR { (Ast.Char $1) }

string:
  | rev_strvals { Ast.String $1 }

rev_strvals:
  | STRING { [$1] }
  | rev_strvals STRING { $2 :: $1 }

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
        Ast.cr_clause_guard = None;
        Ast.cr_clause_arrow = $2;
        Ast.cr_clause_body = $3; } }

  | pattern WHEN guard RARROW body
  { { Ast.cr_clause_ptn = $1;
        Ast.cr_clause_when = Some $2;
        Ast.cr_clause_guard = Some $3;
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

pattern:
  | match_exp { $1 }

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
  | TRY exps OF cr_clauses try_catch
  { Ast.Try {
      try_begin = $1;
      try_exps = $2;
      try_of = Some $3;
      try_clauses = Some $4;
      try_catch = $5;
    }
  }
  | TRY exps try_catch
  { Ast.Try {
      try_begin = $1;
      try_exps = $2;
      try_of = None;
      try_clauses = None;
      try_catch = $3;
    }
  }

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
        try_clause_arrow = $2;
        try_clause_body = $3; }
  }
  | primary_exp WHEN guard RARROW body
  { { Ast.try_clause_exn = None;
        try_clause_exp = $1;
        try_clause_guard = Some ($2, $3);
        try_clause_arrow = $4;
        try_clause_body = $5; }
  }
  | raw_atom COLON exp RARROW body
  { { Ast.try_clause_exn = Some ($1, $2);
        try_clause_exp = $3;
        try_clause_guard = None;
        try_clause_arrow = $4;
        try_clause_body = $5; }
  }
  | raw_atom COLON exp WHEN guard RARROW body
  { { Ast.try_clause_exn = Some ($1, $2);
        try_clause_exp = $3;
        try_clause_guard = Some ($4, $5);
        try_clause_arrow = $6;
        try_clause_body = $7; }
  }
