{
open Lexing
open Parser

exception Syntax_error of Position.t * string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let revise_pos pos lexbuf =
  Position.of_lexing_pos
    { pos with pos_bol = pos.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1 }

let start_pos lexbuf =
  revise_pos (lexeme_start_p lexbuf) lexbuf

let end_pos lexbuf =
  let p = lexeme_end_p lexbuf in
  let p' = { p with pos_cnum = p.pos_cnum - 1 } in
  revise_pos p' lexbuf

let to_loc lexbuf =
  Location.create (start_pos lexbuf) (end_pos lexbuf)

let to_word lexbuf =
  Located.locate (to_loc lexbuf) (lexeme lexbuf)

let strlit lexbuf read =
  let sp = start_pos lexbuf in
  let contents = read (Buffer.create 17) lexbuf in
  let loc = Location.create sp (end_pos lexbuf) in
  Located.locate loc contents

}

let digit = ['0'-'9']
let int = '-'? digit+ ('#' ['a'-'z' 'A'-'Z' '0'-'9']+)?
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ '.' digit+ exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let lower = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let upper = [ 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let char = '$' _
let attr_prefix = '-' white*
let module_attr = attr_prefix "module"
let export_attr = attr_prefix "export"
let import_attr = attr_prefix "import"
let include_attr = attr_prefix "include"
let spec_attr = attr_prefix "spec"

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | char    { CHAR (to_word lexbuf) }
  | int     { INT (to_word lexbuf) }
  | float   { FLOAT (to_word lexbuf) }
  | '"'     { STRING (strlit lexbuf read_string) } 
  | '('     { LPAREN (to_loc lexbuf) }
  | ')'     { RPAREN (to_loc lexbuf) }
  | '{'     { LBRACE (to_loc lexbuf) }
  | '}'     { RBRACE (to_loc lexbuf) }
  | '['     { LBRACK (to_loc lexbuf) }
  | ']'     { RBRACK (to_loc lexbuf) }
  | ':'     { COLON (to_loc lexbuf) }
  | ';'     { SEMI (to_loc lexbuf) }
  | ','     { COMMA (to_loc lexbuf) }
  | '.'     { DOT (to_loc lexbuf) }
  | "..."   { DOT3 (to_loc lexbuf) }
  | '<'     { LT (to_loc lexbuf) }
  | '>'     { GT (to_loc lexbuf) }
  | "=<"    { LE (to_loc lexbuf) }
  | ">="    { GE (to_loc lexbuf) }
  | '#'     { NSIGN (to_loc lexbuf) }
  | '+'     { PLUS (to_loc lexbuf) }
  | '-'     { MINUS (to_loc lexbuf) }
  | '*'     { MUL (to_loc lexbuf) }
  | '/'     { DIV (to_loc lexbuf) }
  | '_'     { USCORE (to_loc lexbuf) }
  | '='     { MATCH (to_loc lexbuf) }
  | '!'     { SEND (to_loc lexbuf) }
  | '?'     { Q (to_loc lexbuf) }
  | '|'     { BAR (to_loc lexbuf) }
  | "||"    { DBAR (to_loc lexbuf) }
  | "++"    { LIST_ADD (to_loc lexbuf) }
  | "--"    { LIST_DIFF (to_loc lexbuf) }
  | "->"    { RARROW (to_loc lexbuf) }
  | "<-"    { LARROW (to_loc lexbuf) }
  | "<="    { LARROW2 (to_loc lexbuf) }
  | ">>"    { DLT (to_loc lexbuf) }
  | "<<"    { DGT (to_loc lexbuf) }
  | "after" { AFTER (to_loc lexbuf) }
  | "begin" { BEGIN (to_loc lexbuf) }
  | "case"  { CASE (to_loc lexbuf) }
  | "catch" { CATCH (to_loc lexbuf) }
  | "end"   { END (to_loc lexbuf) }
  | "fun"   { FUN (to_loc lexbuf) }
  | "if"    { IF (to_loc lexbuf) }
  | "of"    { OF (to_loc lexbuf) }
  | "receive" { RECEIVE (to_loc lexbuf) }
  | "try"   { TRY (to_loc lexbuf) }
  | lower   { LIDENT (to_word lexbuf) }
  | upper   { UIDENT (to_word lexbuf) }
  | module_attr { MODULE_ATTR (to_word lexbuf) }
  | export_attr { EXPORT_ATTR (to_word lexbuf) }
  | import_attr { IMPORT_ATTR (to_word lexbuf) }
  | include_attr { INCLUDE_ATTR (to_word lexbuf) }
  | spec_attr { SPEC_ATTR (to_word lexbuf) }
  | _       { raise (Syntax_error (start_pos lexbuf, "Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }

and read_string buf =
  parse
  | '"'       { Buffer.contents buf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Syntax_error (start_pos lexbuf, "Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Syntax_error (start_pos lexbuf, "String is not terminated")) }
