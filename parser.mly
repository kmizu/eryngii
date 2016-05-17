%token <string> UIDENT
%token <string> LIDENT
%token <string> CHAR
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token COMMA
%token DOT
%token COLON
%token SEMI
%token USCORE           (* "_" *)
%token NSIGN            (* "#" *)
%token AND              (* "and" *)
%token OR               (* "or" *)
%token NOT              (* "not" *)
%token LAND             (* "band" *)
%token LOR              (* "bor" *)
%token LXOR             (* "bxor" *)
%token LNOT             (* "bnot" *)
%token LSHIFT           (* "bsl" *)
%token RSHIFT           (* "bsr" *)
%token MATCH            (* "=" *)
%token SEND             (* "!" *)
%token BAR              (* "|" *)
%token DBAR             (* "||" *)
%token EQ
%token NE
%token XEQ
%token XNE
%token LT
%token LE
%token GT
%token GE
%token ADD
%token SUB
%token MUL              (* "*" *)
%token DIV              (* "/" *)
%token QUO              (* "div" *)
%token REM              (* "rem" *)
%token LIST_ADD         (* "++" *)
%token LIST_DIFF        (* "--" *)
%token RARROW           (* "->" *)
%token LARROW           (* "<-" *)
%token MODULE           (* "-module" *)
%token EXPORT           (* "-export" *)
%token BEGIN            (* "begin" *)
%token END              (* "end" *)
%token AFTER            (* "after" *)
%token COND             (* "cond" *)
%token LET              (* "let" *)
%token WHEN             (* "when" *)
%token OF               (* "of" *)
%token CASE             (* "case" *)
%token FUN              (* "fun" *)
%token QUERY            (* "query" *)
%token CATCH            (* "catch" *)
%token IF               (* "if" *)
%token RECEIVE          (* "receive" *)
%token EOF

%start <Syntax.ast option> prog

%%

prog:
  | EOF { None }
  | module_decl { Some $1 }
  ;

module_decl:
  | attrs forms EOF { Syntax.Prog ($1, $2) }
  ;

attrs:
  | attr { [$1] }
  | attr attrs { [$1] @ $2 }
  ;

(* TODO *)
attr:
  | mod_attr { $1 }
  | export_attr { $1 }
  ;

mod_attr:
  | MODULE LPAREN LIDENT RPAREN { Syntax.ModAttr($3) }
  ;

export_attr:
  | EXPORT LPAREN export_funcs_opt RPAREN { Syntax.ExportAttr($3) }
  ;

export_funcs_opt:
  | LBRACK RBRACK { [] }
  | LBRACK export_funcs RBRACK { $2 }
  ;

export_funcs:
  | export_func { [$1] }
  | export_func COMMA export_funcs { [$1] @ $3 }
  ;

export_func:
  | LIDENT DIV INT { { Syntax.type_name = $1; Syntax.type_arity = $3; } }
  ;

forms:
  | form { [$1] }
  | forms form { $1 @ [$2] }
  ;

form:
  | func_decl { $1 }
  ;

func_decl:
  | func_clauses DOT { Syntax.FuncDecl $1 }
  ;

func_clauses:
  | func_clause { [$1] }
  | func_clauses SEMI func_clause { $1 @ [$3] }
  ;

func_clause:
  | LIDENT func_clause_def
    { { $2 with Syntax.func_clause_name = Some $1 } }
  ;

func_clause_def:
  | LPAREN patterns_opt RPAREN clause_guard_opt clause_body
    { { Syntax.func_clause_name = None;
        Syntax.func_clause_patterns = $2;
        Syntax.func_clause_guard = $4;
        Syntax.func_clause_body = $5; } }
  ;

clause_guard_opt:
  | clause_guard { $1 }
  | (* empty *) { [] }
  ;

clause_guard:
  | WHEN guard { $2 }
  ;

guard:
  | guard_test { [$1] }
  | guard COMMA guard_test { $1 @ [$3] }
  ;

guard_test:
  | atom { $1 } (* true only *)
  | guard_recognizer { $1 }
  | guard_term_comparison { $1 }
  | LPAREN guard_test RPAREN { Syntax.GuardParenTest $2 }
  ;

guard_recognizer:
  | LIDENT LPAREN guard_expr RPAREN { Syntax.GuardFuncCallExpr ($1, $3) }
  ;

guard_term_comparison:
  | guard_expr compare_op guard_expr { Syntax.GuardBinExpr ($1, $2, $3) }
  ;

guard_expr:
  | guard_shift_expr { $1 }
  ;
 
guard_shift_expr:
  | guard_shift_expr shift_op guard_mul_expr { Syntax.GuardBinExpr ($1, $2, $3) }
  | guard_mul_expr { $1 }
  ;

guard_mul_expr:
  | guard_mul_expr mul_op guard_prefix_expr { Syntax.GuardBinExpr ($1, $2, $3) }
  ;

guard_prefix_expr:
  | prefix_op guard_app_expr { Syntax.GuardUnaryExpr ($2, $1) }
  | guard_app_expr { $1 }
  ;

guard_app_expr:
  | LIDENT LPAREN guard_exprs_opt RPAREN { Syntax.GuardAppExpr ($1, $3) }
  | guard_record_expr { $1 }
  | guard_primary_expr { $1 }
  ;

guard_exprs_opt:
  | guard_exprs { $1 }
  | (* empty *) { [] }
  ;

guard_exprs:
  | guard_expr { [$1] }
  | guard_exprs COMMA guard_expr { $1 @ [$3] }
  ;

guard_record_expr:
  | guard_primary_expr_opt NSIGN LIDENT DOT LIDENT
    { Syntax.GuardRecordExpr ($1, $3, $5) }
  ;

guard_primary_expr_opt:
  | guard_primary_expr { Some $1 }
  | (* empty *) { None }
  ;

guard_primary_expr:
  | var { $1 }
  | atomic { $1 }
  | guard_list_skel { $1 }
  | guard_tuple_skel { $1 }
  | LPAREN guard_expr RPAREN { Syntax.GuardParenExpr $2 }
  ;

guard_list_skel:
  | LBRACK RBRACK { Syntax.GuardList [] }
  | LBRACK guard_exprs guard_list_skel_tail_opt RBRACK
    { Syntax.GuardList ($2, $3) }
  ;

guard_list_skel_tail_opt:
  | BAR guard_expr { Some $2 }
  | (* empty *) { None }
  ;

guard_tuple_skel:
  | LBRACE guard_exprs_opt RBRACE { Syntax.GuardTuple $2 }
  ;

clause_body:
  | RARROW body { $2 }
  ;

body:
  | exprs { Syntax.Body $1 }
  ;

exprs:
  | expr { [$1] }
  | exprs COMMA expr { $1 @ [$3] }
  ;

exprs_opt:
  | exprs { $1 }
  | (* empty *) { [] }
  ;

expr:
  | CATCH expr { Syntax.CatchExpr $2 }
  | match_expr { $1 }
  ;

match_expr:
  | pattern MATCH match_expr { Syntax.BinExpr($1, Syntax.OpMatch, $3) }
  | send_expr { $1 }
  ;

pattern:
  (*| atomic { $1 }*)
  (*| var { $1 }*)
  | universal_pattern { $1 }
  (*| tuple_pattern { $1 }*)
  (*| record_pattern { $1 }*)
  (*| list_pattern { $1 }*)
  ;

universal_pattern:
  | USCORE { Syntax.Universal }
  ;

tuple_pattern:
  | LBRACE patterns_opt RBRACE { Syntax.Tuple $2 }
  ;

list_pattern:
  | LBRACK RBRACK { Syntax.List ([], None) }
  | LBRACK patterns list_pattern_tail_opt RBRACK { Syntax.List ($2, $3) }
  ;

list_pattern_tail_opt:
  | pattern { Some $1 }
  | (* empty *) { None }
  ;

patterns:
  | pattern { [$1] }
  | patterns COMMA pattern { $1 @ [$3] }
  ;

patterns_opt:
  | patterns { $1 }
  | (* empty *) { [] }
  ;

record_pattern:
  | NSIGN record_type record_pattern_tuple { Syntax.Record ($2, $3) }
  ;

record_type:
  | atom { $1 }
  ;

record_pattern_tuple:
  | LBRACE record_field_patterns RBRACE { $2 }
  | LBRACE RBRACE { [] }
  ;

record_field_patterns:
  | record_field_pattern { [$1] }
  | record_field_patterns COMMA record_field_pattern { $1 @ [$3] }
  ;

record_field_pattern:
  | record_field_name MATCH pattern { Syntax.Assoc ($1, $3) }
  ;

record_field_name:
  | atom { $1 }
  ;

send_expr:
  | compare_expr SEND send_expr { Syntax.BinExpr($1, Syntax.OpSend, $3) }
  | compare_expr { $1 }
  ;

compare_expr:
  | list_conc_expr compare_op list_conc_expr { Syntax.BinExpr($1, $2, $3) }
  | list_conc_expr { $1 }
  ;

compare_op:
  | EQ { Syntax.OpEq }
  | NE { Syntax.OpNe }
  | XEQ { Syntax.OpXEq }
  | XNE { Syntax.OpXNe }
  | GT { Syntax.OpGt }
  | GE { Syntax.OpGe }
  | LT { Syntax.OpLt }
  | LE { Syntax.OpLe }
  ;

list_conc_expr:
  | shift_expr list_conc_op list_conc_expr { Syntax.BinExpr($1, $2, $3) }
  | shift_expr { $1 }
  ;

list_conc_op:
  | LIST_ADD { Syntax.OpListAdd }
  | LIST_DIFF { Syntax.OpListDiff }
  ;

shift_expr:
  | shift_expr shift_op mul_expr { Syntax.BinExpr($1, $2, $3) }
  | mul_expr { $1 }
  ;
  
shift_op:
  | ADD { Syntax.OpAdd }
  | SUB { Syntax.OpSub }
  | LOR { Syntax.OpLOr }
  | LXOR { Syntax.OpLXor }
  | LSHIFT { Syntax.OpLShift }
  | RSHIFT { Syntax.OpRShift }
  ;

mul_expr:
  | mul_expr mul_op prefix_expr { Syntax.BinExpr($1, $2, $3) }
  | mul_expr AND prefix_expr { Syntax.BinExpr($1, Syntax.OpAnd, $3) }
  | prefix_expr { $1 }
  ;

mul_op:
  | MUL { Syntax.OpMul }
  | DIV { Syntax.OpDiv }
  | QUO { Syntax.OpQuo }
  | REM { Syntax.OpRem }
  | LAND { Syntax.OpLAnd }
  ;

prefix_expr:
  | prefix_op record_expr { Syntax.UnaryExpr($2, $1) }
  | record_expr { $1 }
  ;

prefix_op:
  | ADD { Syntax.OpPos }
  | SUB { Syntax.OpNeg }
  | NOT { Syntax.OpNot }
  | LNOT { Syntax.OpLNot }
  ;

record_expr:
(*
  | record_expr_opt NSIGN record_type DOT record_field_name
    { Syntax.RecordCreationExpr ($1, $3, $5) }
  | record_expr_opt NSIGN record_type record_update_tuple
    { Syntax.RecordAccessExpr ($1, $3, $4) }
*)
  | app_expr { $1 }
  ;

record_expr_opt:
  | record_expr { Some $1 }
  | (* empty *) { None }
  ;

record_update_tuple:
  | LBRACE record_field_updates_opt RBRACE { $2 }
  ;

record_field_updates_opt:
  | record_field_updates { $1 }
  | (* empty *) { [] }
  ;

record_field_updates:
  | record_field_update { [$1] }
  | record_field_updates COMMA record_field_update { $1 @ [$3] }
  ;

record_field_update:
  | record_field_name MATCH expr { Syntax.Assoc ($1, $3) }
  ;

app_expr:
  | primary_expr LPAREN exprs_opt RPAREN
    { Syntax.FuncCallExpr { Syntax.func_call_mod = None;
      Syntax.func_call_func = $1; Syntax.func_call_args = $3; } }
  | primary_expr COLON primary_expr LPAREN exprs_opt RPAREN
    { Syntax.FuncCallExpr { Syntax.func_call_mod = Some $1;
      Syntax.func_call_func = $3; Syntax.func_call_args = $5; } }
  | primary_expr { $1 }
  ;

(* TODO *)
primary_expr:
  | var { $1 }
  | atomic { $1 }
  (*| tuple_skel { $1 }*)
  | list_skel { $1 }
  | list_comprehension { $1 }
  | block_expr { $1 }
  | if_expr { $1 }
  | case_expr { $1 }
  | receive_expr { $1 }
  | func_expr { $1 }
  | query_expr { $1 }
  | LPAREN expr RPAREN { Syntax.ParenExpr $2 }
  ;

var:
  | UIDENT { Syntax.Var($1) }
  ;

atomic:
  | atom { $1 }
  | char { $1 }
  | string { Syntax.String [$1] }
  | integer { $1 }
  | float { $1 }
  ;

atom:
  | LIDENT { Syntax.Atom($1) }
  ;

char:
  | CHAR { Syntax.Char($1) }
  ;

string:
  | STRING { Syntax.String $1 }
  ;

integer:
  | INT { Syntax.Int($1) }
  ;

float:
  | FLOAT { Syntax.Float($1) }
  ;

tuple_skel:
  | LBRACE exprs_opt RBRACE { Syntax.Tuple $2 }
  ;
  
list_skel:
  | LBRACK RBRACK { Syntax.List [] }
  | LBRACK exprs list_skel_tail_opt RBRACK { Syntax.List ($2, $3) }
  ;
  
list_skel_tail_opt:
  | BAR expr { Some $2 }
  | (* empty *) { None }
  ;

list_comprehension:
  | LBRACK expr DBAR list_comprehension_exprs RBRACK
    { Syntax.ListComp ($2, $4) }
  ;
  
list_comprehension_exprs:
  | list_comprehension_expr { [$1] }
  | list_comprehension_exprs COMMA list_comprehension_expr { $1 @ [$3] }
  ;
  
list_comprehension_expr:
  | generator { $1 }
  | filter { $1 }
  ;
  
generator:
  | pattern LARROW expr { Syntax.Generator ($1, $3) }
  
filter:
  | expr { $1 }
  ;
  
block_expr:
  | BEGIN body END { Syntax.BlockExpr $2 }
  ;
  
if_expr:
  | IF if_clauses END { Syntax.IfExpr $2 }
  ;
  
if_clauses:
  | if_clause { [$1] }
  | if_clauses SEMI if_clause { $1 @ [$3] }
  ;
  
if_clause:
  | guard clause_body { ($1, $2) }
  ;
  
case_expr:
  | CASE expr OF cr_clauses END { Syntax.CaseExpr ($2, $4) }
  ;
  
cr_clauses:
  | cr_clause { [$1] }
  | cr_clauses SEMI cr_clause { $1 @ [$3] }
  ;
  
cr_clause:
  | pattern clause_guard_opt clause_body
    { { Syntax.cr_clause_pattern = $1;
        Syntax.cr_clause_gaurd = $2;
        Syntax.cr_clause_body = $3; } }
  ;
  
receive_expr:
  | RECEIVE cr_clauses END { Syntax.ReceiveExpr ($2, None) }
  | RECEIVE cr_clauses_opt AFTER expr clause_body END
    { Syntax.ReceiveExpr ($2, Some ($4, $5)) }
  ;
  
cr_clauses_opt:
  | cr_clauses { $1 }
  | (* empty *) { [] }
  ;
  
func_expr:
  | FUN func_arity { Syntax.FuncExpr $2 }
  | FUN func_clauses END { Syntax.FuncExpr $2 }
  ;
  
func_arity:
  | primary_expr COLON primary_expr DIV INT
    { Syntax.FuncArity (Some $1, $3, $5) }
  | primary_expr DIV INT { Syntax.FuncArity (None, $1, $3) }
  ;

query_expr:
  | QUERY list_comprehension END { Syntax.QueryExpr $2 }
  ;
  
