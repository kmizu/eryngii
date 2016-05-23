%token <Ast.token> UIDENT
%token <Ast.token> LIDENT
%token <Ast.token> CHAR
%token <Ast.token> STRING
%token <Ast.token> INT
%token <Ast.token> FLOAT
%token <Ast.token> LPAREN
%token <Ast.token> RPAREN
%token <Ast.token> LBRACK
%token <Ast.token> RBRACK
%token <Ast.token> LBRACE
%token <Ast.token> RBRACE
%token <Ast.token> COMMA
%token <Ast.token> DOT
%token <Ast.token> COLON
%token <Ast.token> SEMI
%token <Ast.token> USCORE           (* "_" *)
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
%token <Ast.token> MATCH            (* "=" *)
%token <Ast.token> SEND             (* "!" *)
%token <Ast.token> BAR              (* "|" *)
%token <Ast.token> DBAR             (* "||" *)
%token <Ast.token> EQ
%token <Ast.token> NE
%token <Ast.token> XEQ
%token <Ast.token> XNE
%token <Ast.token> LT
%token <Ast.token> LE
%token <Ast.token> GT
%token <Ast.token> GE
%token <Ast.token> PLUS
%token <Ast.token> MINUS
%token <Ast.token> MUL              (* "*" *)
%token <Ast.token> DIV              (* "/" *)
%token <Ast.token> QUO              (* "div" *)
%token <Ast.token> REM              (* "rem" *)
%token <Ast.token> LIST_ADD         (* "++" *)
%token <Ast.token> LIST_DIFF        (* "--" *)
%token <Ast.token> RARROW           (* "->" *)
%token <Ast.token> LARROW           (* "<-" *)
%token <Ast.token> MODULE           (* "-module" *)
%token <Ast.token> EXPORT           (* "-export" *)
%token <Ast.token> BEGIN            (* "begin" *)
%token <Ast.token> END              (* "end" *)
%token <Ast.token> AFTER            (* "after" *)
%token <Ast.token> COND             (* "cond" *)
%token <Ast.token> LET              (* "let" *)
%token <Ast.token> WHEN             (* "when" *)
%token <Ast.token> OF               (* "of" *)
%token <Ast.token> CASE             (* "case" *)
%token <Ast.token> FUN              (* "fun" *)
%token <Ast.token> QUERY            (* "query" *)
%token <Ast.token> CATCH            (* "catch" *)
%token <Ast.token> IF               (* "if" *)
%token <Ast.token> RECEIVE          (* "receive" *)
%token EOF

%start <Ast.t option> module_

%%

module_:
  | EOF { None }
  | module_decls { Some $1 }

module_decls:
  | module_attrs fun_decls EOF
  { Ast.Module {
      module_attrs = $1;
      module_decls = $2; }
  }


module_attrs:
  | module_attr* { $1 }

module_attr:
  | MINUS LIDENT LPAREN exp RPAREN DOT { $1 }

fun_decls:
  | fun_decl* { $1 }

fun_decl:
  | fun_clauses DOT { Ast.funDecl $1 }

fun_clauses:
  | fun_clause { [$1] }
  | fun_clauses SEMI fun_clause { $1 @ [$3] }

fun_clause:
  | LIDENT fun_clause_def
    { { $2 with Ast.fun_clause_name = Some $1 } }

fun_clause_def:
  | LPAREN patterns_opt RPAREN guard_clause_opt clause_body
    { { Ast.fun_clause_name = None;
        Ast.fun_clause_patterns = $2;
        Ast.fun_guard_clause = $4;
        Ast.fun_clause_body = $5; } }

guard_clause_opt:
  | guard_clause { $1 }
  | (* empty *) { [] }

guard_clause:
  | WHEN guard { $2 }

guard:
  | guard_test { [$1] }
  | guard COMMA guard_test { $1 @ [$3] }

guard_test:
  | atom { $1 } (* true only *)
  | guard_recognizer { $1 }
  | guard_term_comparison { $1 }
  | LPAREN guard_test RPAREN { Ast.GuardParenTest $2 }

guard_recognizer:
  | LIDENT LPAREN guard_exp RPAREN { Ast.Guardfunallexp ($1, $3) }

guard_term_comparison:
  | guard_exp compare_op guard_exp { Ast.GuardBinexp ($1, $2, $3) }

guard_exp:
  | guard_shift_exp { $1 }

guard_shift_exp:
  | guard_shift_exp shift_op guard_mul_exp { Ast.GuardBinexp ($1, $2, $3) }
  | guard_mul_exp { $1 }

guard_mul_exp:
  | guard_mul_exp mul_op guard_prefix_exp { Ast.GuardBinexp ($1, $2, $3) }

guard_prefix_exp:
  | prefix_op guard_app_exp { Ast.GuardUnaryexp ($2, $1) }
  | guard_app_exp { $1 }

guard_app_exp:
  | LIDENT LPAREN guard_exps_opt RPAREN { Ast.GuardAppexp ($1, $3) }
  | guard_record_exp { $1 }
  | guard_primary_exp { $1 }

guard_exps_opt:
  | guard_exps { $1 }
  | (* empty *) { [] }

guard_exps:
  | guard_exp { [$1] }
  | guard_exps COMMA guard_exp { $1 @ [$3] }

guard_record_exp:
  | guard_primary_exp_opt NSIGN LIDENT DOT LIDENT
    { Ast.GuardRecordexp ($1, $3, $5) }

guard_primary_exp_opt:
  | guard_primary_exp { Some $1 }
  | (* empty *) { None }

guard_primary_exp:
  | var { $1 }
  | atomic { $1 }
  | guard_list_skel { $1 }
  | guard_tuple_skel { $1 }
  | LPAREN guard_exp RPAREN { Ast.GuardParenexp $2 }

guard_list_skel:
  | LBRACK RBRACK { Ast.GuardList [] }
  | LBRACK guard_exps guard_list_skel_tail_opt RBRACK
    { Ast.GuardList ($2, $3) }

guard_list_skel_tail_opt:
  | BAR guard_exp { Some $2 }
  | (* empty *) { None }

guard_tuple_skel:
  | LBRACE guard_exps_opt RBRACE { Ast.GuardTuple $2 }

body:
  | exps { Ast.Body $1 }

exps:
  | exp { [$1] }
  | exps COMMA exp { $1 @ [$3] }

exps_opt:
  | exps { $1 }
  | (* empty *) { [] }

exp:
  | CATCH exp { Ast.Catchexp $2 }
  | match_exp { $1 }

match_exp:
  | pattern MATCH match_exp { Ast.Binexp($1, Ast.OpMatch, $3) }
  | send_exp { $1 }

pattern:
  (*| atomic { $1 }*)
  (*| var { $1 }*)
  | universal_pattern { $1 }
  (*| tuple_pattern { $1 }*)
  (*| record_pattern { $1 }*)
  (*| list_pattern { $1 }*)

universal_pattern:
  | USCORE { Ast.Universal }

tuple_pattern:
  | LBRACE patterns_opt RBRACE { Ast.Tuple $2 }

list_pattern:
  | LBRACK RBRACK { Ast.List ([], None) }
  | LBRACK patterns list_pattern_tail_opt RBRACK { Ast.List ($2, $3) }

list_pattern_tail_opt:
  | pattern { Some $1 }
  | (* empty *) { None }

patterns:
  | pattern { [$1] }
  | patterns COMMA pattern { $1 @ [$3] }

patterns_opt:
  | patterns { $1 }
  | (* empty *) { [] }

record_pattern:
  | NSIGN record_type record_pattern_tuple { Ast.Record ($2, $3) }

record_type:
  | atom { $1 }

record_pattern_tuple:
  | LBRACE record_field_patterns RBRACE { $2 }
  | LBRACE RBRACE { [] }

record_field_patterns:
  | record_field_pattern { [$1] }
  | record_field_patterns COMMA record_field_pattern { $1 @ [$3] }

record_field_pattern:
  | record_field_name MATCH pattern { Ast.Assoc ($1, $3) }

record_field_name:
  | atom { $1 }

send_exp:
  | compare_exp SEND send_exp { Ast.Binexp($1, Ast.OpSend, $3) }
  | compare_exp { $1 }

compare_exp:
  | list_conc_exp compare_op list_conc_exp { Ast.Binexp($1, $2, $3) }
  | list_conc_exp { $1 }

compare_op:
  | EQ { Ast.OpEq }
  | NE { Ast.OpNe }
  | XEQ { Ast.OpXEq }
  | XNE { Ast.OpXNe }
  | GT { Ast.OpGt }
  | GE { Ast.OpGe }
  | LT { Ast.OpLt }
  | LE { Ast.OpLe }

list_conc_exp:
  | shift_exp list_conc_op list_conc_exp { Ast.Binexp($1, $2, $3) }
  | shift_exp { $1 }

list_conc_op:
  | LIST_ADD { Ast.OpListAdd }
  | LIST_DIFF { Ast.OpListDiff }

shift_exp:
  | shift_exp shift_op mul_exp { Ast.Binexp($1, $2, $3) }
  | mul_exp { $1 }

shift_op:
  | PLUS { Ast.Op_add }
  | MINUS { Ast.Op_sub }
  | LOR { Ast.Op_lor }
  | LXOR { Ast.Op_lxor }
  | LSHIFT { Ast.Op_lshift }
  | RSHIFT { Ast.Op_rshift }

mul_exp:
  | mul_exp mul_op prefix_exp { Ast.Binexp($1, $2, $3) }
  | mul_exp AND prefix_exp { Ast.Binexp($1, Ast.OpAnd, $3) }
  | prefix_exp { $1 }

mul_op:
  | MUL { Ast.Op_mul }
  | DIV { Ast.Op_div }
  | QUO { Ast.Op_quo }
  | REM { Ast.Op_rem }
  | LAND { Ast.Op_land }

prefix_exp:
  | prefix_op record_exp { Ast.Unaryexp($2, $1) }
  | record_exp { $1 }

prefix_op:
  | PLUS { Ast.Op_pos }
  | MINUS { Ast.Op_neg }
  | NOT { Ast.Op_not }
  | LNOT { Ast.Op_lnot }

record_exp:
(*
  | record_exp_opt NSIGN record_type DOT record_field_name
    { Ast.RecordCreationexp ($1, $3, $5) }
  | record_exp_opt NSIGN record_type record_update_tuple
    { Ast.RecordAccessexp ($1, $3, $4) }
*)
  | app_exp { $1 }

record_exp_opt:
  | record_exp { Some $1 }
  | (* empty *) { None }

record_update_tuple:
  | LBRACE record_field_updates_opt RBRACE { $2 }

record_field_updates_opt:
  | record_field_updates { $1 }
  | (* empty *) { [] }

record_field_updates:
  | record_field_update { [$1] }
  | record_field_updates COMMA record_field_update { $1 @ [$3] }

record_field_update:
  | record_field_name MATCH exp { Ast.Assoc ($1, $3) }

app_exp:
  | primary_exp LPAREN exps_opt RPAREN
    { Ast.funallexp { Ast.fun_call_mod = None;
      Ast.fun_call_fun = $1; Ast.fun_call_args = $3; } }
  | primary_exp COLON primary_exp LPAREN exps_opt RPAREN
    { Ast.funallexp { Ast.fun_call_mod = Some $1;
      Ast.fun_call_fun = $3; Ast.fun_call_args = $5; } }
  | primary_exp { $1 }

(* TODO *)
primary_exp:
  | var { $1 }
  | atomic { $1 }
  | tuple_skel { $1 }
  | list_skel { $1 }
  | list_compr { $1 }
  | block_exp { $1 }
  | if_exp { $1 }
  | case_exp { $1 }
  | receive_exp { $1 }
  | fun_exp { $1 }
  | query_exp { $1 }
  | LPAREN exp RPAREN { Ast.Parenexp $2 }

var:
  | UIDENT { Ast.Var($1) }

atomic:
  | atom { $1 }
  | char { $1 }
  | string { Ast.String [$1] }
  | integer { $1 }
  | float { $1 }

atom:
  | LIDENT { Ast.Atom($1) }

char:
  | CHAR { Ast.Char($1) }

string:
  | STRING { Ast.String $1 }

integer:
  | INT { Ast.Int($1) }

float:
  | FLOAT { Ast.Float($1) }

tuple_skel:
  | LBRACE exps_opt RBRACE { Ast.Tuple $2 }

list_skel:
  | LBRACK RBRACK { Ast.List [] }
  | LBRACK exps list_skel_tail_opt RBRACK { Ast.List ($2, $3) }

list_skel_tail_opt:
  | BAR exp { Some $2 }
  | (* empty *) { None }

list_compr:
  | LBRACK exp DBAR list_compr_quals RBRACK
  { Ast.List_compr ($2, $4) }

list_compr_quals:
  | list_compr_qual { [$1] }
  | list_compr_quals COMMA list_compr_qual { $1 @ [$3] }

list_compr_qual:
  | list_compr_generator { $1 }
  | list_compr_filter { $1 }

list_compr_generator:
  | pattern LARROW exp
  { Ast.List_compr_gen {
      gen_ptn = $1;
      gen_arrow = $2;
      gen_exp = $3 }
  }

list_compr_filter:
  | exp { $1 }

block_exp:
  | BEGIN body END { Ast.Blockexp $2 }

if_exp:
  | IF if_clauses END { Ast.Ifexp $2 }

if_clauses:
  | if_clause { [$1] }
  | if_clauses SEMI if_clause { $1 @ [$3] }

if_clause:
  | guard clause_body { ($1, $2) }

clause_body:
  | RARROW body { $2 }

case_exp:
  | CASE exp OF case_clauses END { Ast.Caseexp ($2, $4) }

case_clauses:
  | case_clause { [$1] }
  | case_clauses SEMI case_clause { $1 @ [$3] }

case_clause:
  | pattern guard_clause_opt clause_body
    { { Ast.case_clause_pattern = $1;
        Ast.case_clause_gaurd = $2;
        Ast.case_clause_body = $3; } }

receive_exp:
  | RECEIVE case_clauses END { Ast.Receiveexp ($2, None) }
  | RECEIVE case_clauses_opt AFTER exp clause_body END
    { Ast.Receiveexp ($2, Some ($4, $5)) }

case_clauses_opt:
  | case_clauses { $1 }
  | (* empty *) { [] }

fun_exp:
  | FUN fun_arity { Ast.Fun $2 }
  | FUN fun_clauses END { Ast.Fun $2 }

fun_arity:
  | primary_exp COLON primary_exp DIV INT
    { Ast.funArity (Some $1, $3, $5) }
  | primary_exp DIV INT { Ast.funArity (None, $1, $3) }

query_exp:
  | QUERY list_compr END { Ast.Queryexp $2 }
