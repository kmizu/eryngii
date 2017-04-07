{
open Lexing
open Parser

exception Syntax_error of Position.t * string

let to_pos pos lexbuf =
  Position.of_lexing_pos pos

let start_pos lexbuf =
  to_pos (lexeme_start_p lexbuf) lexbuf

let end_pos lexbuf =
  to_pos (lexeme_end_p lexbuf) lexbuf

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
let uscore = '_' ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let char = '$' ('\\' ['b' 'd' 'e' 'f' 'n' 's' 'r' 't' 'v' '\'' '"' '\\'] | _)
let attr = '-' white*
let comment = '%' [^'\r' '\n']*

rule read =
  parse
  | white   { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | comment { Annot.add_comment (to_word lexbuf); read lexbuf }
  | char    { CHAR (to_word lexbuf) }
  | int     { INT (to_word lexbuf) }
  | float   { FLOAT (to_word lexbuf) }
  | '\''    { ATOM (strlit lexbuf read_atom) } 
  | '"'     { STRING (strlit lexbuf read_string) } 
  | '('     { LPAREN (to_loc lexbuf) }
  | ')'     { RPAREN (to_loc lexbuf) }
  | '{'     { LBRACE (to_loc lexbuf) }
  | '}'     { RBRACE (to_loc lexbuf) }
  | '['     { LBRACK (to_loc lexbuf) }
  | ']'     { RBRACK (to_loc lexbuf) }
  | ':'     { COLON (to_loc lexbuf) }
  | "::"    { COLON2 (to_loc lexbuf) }
  | ';'     { SEMI (to_loc lexbuf) }
  | ','     { COMMA (to_loc lexbuf) }
  | '.'     { DOT (to_loc lexbuf) }
  | ".."    { DOT2 (to_loc lexbuf) }
  | "..."   { DOT3 (to_loc lexbuf) }
  | "=="    { EQQ (to_loc lexbuf) }
  | ":="    { CEQ (to_loc lexbuf) }
  | "/="    { NE (to_loc lexbuf) }
  | "=:="   { XEQ (to_loc lexbuf) }
  | "=/="   { XNE (to_loc lexbuf) }
  | '<'     { LT (to_loc lexbuf) }
  | '>'     { GT (to_loc lexbuf) }
  | "=<"    { LE (to_loc lexbuf) }
  | ">="    { GE (to_loc lexbuf) }
  | '#'     { NSIGN (to_loc lexbuf) }
  | '+'     { PLUS (to_loc lexbuf) }
  | '-'     { MINUS (to_loc lexbuf) }
  | '*'     { MUL (to_loc lexbuf) }
  | '/'     { DIV (to_loc lexbuf) }
  | '='     { EQ (to_loc lexbuf) }
  | '!'     { BANG (to_loc lexbuf) }
  | '?'     { Q (to_loc lexbuf) }
  | '|'     { BAR (to_loc lexbuf) }
  | "||"    { DBAR (to_loc lexbuf) }
  | "++"    { PLUS2 (to_loc lexbuf) }
  | "--"    { MINUS2 (to_loc lexbuf) }
  | "->"    { RARROW (to_loc lexbuf) }
  | "=>"    { RARROW2 (to_loc lexbuf) }
  | "<-"    { LARROW (to_loc lexbuf) }
  | "<="    { LARROW2 (to_loc lexbuf) }
  | ">>"    { DLT (to_loc lexbuf) }
  | "<<"    { DGT (to_loc lexbuf) }
  | "after" { AFTER (to_loc lexbuf) }
  | "and"   { AND (to_loc lexbuf) }
  | "andalso" { ANDALSO (to_loc lexbuf) }
  | "band"  { LAND (to_loc lexbuf) }
  | "begin" { BEGIN (to_loc lexbuf) }
  | "bnot"  { LNOT (to_loc lexbuf) }
  | "bor"   { LOR (to_loc lexbuf) }
  | "bxor"  { LXOR (to_loc lexbuf) }
  | "case"  { CASE (to_loc lexbuf) }
  | "catch" { CATCH (to_loc lexbuf) }
  | "cond"  { COND (to_loc lexbuf) }
  | "div"   { DIV (to_loc lexbuf) }
  | "end"   { END (to_loc lexbuf) }
  | "fun"   { FUN (to_loc lexbuf) }
  | "if"    { IF (to_loc lexbuf) }
  | "let"   { LET (to_loc lexbuf) }
  | "of"    { OF (to_loc lexbuf) }
  | "or"    { OR (to_loc lexbuf) }
  | "oralse" { ORELSE (to_loc lexbuf) }
  | "receive" { RECEIVE (to_loc lexbuf) }
  | "rem"   { REM (to_loc lexbuf) }
  | "try"   { TRY (to_loc lexbuf) }
  | "when"  { WHEN (to_loc lexbuf) }
  | lower   { LIDENT (to_word lexbuf) }
  | upper   { UIDENT (to_word lexbuf) }
  | uscore  { USCORE (to_word lexbuf) }
  | attr "module" { MODULE_ATTR (to_word lexbuf) }
  | attr "export" { EXPORT_ATTR (to_word lexbuf) }
  | attr "export_type" { EXPORT_TYPE_ATTR (to_word lexbuf) }
  | attr "import" { IMPORT_ATTR (to_word lexbuf) }
  | attr "include"{ INCLUDE_ATTR (to_word lexbuf) }
  | attr "include_lib"{ INCLIB_ATTR (to_word lexbuf) }
  | attr "spec" { SPEC_ATTR (to_word lexbuf) }
  | attr "type" { TYPE_ATTR (to_word lexbuf) }
  | attr "opaque" { OPAQUE_ATTR (to_word lexbuf) }
  | attr "define" { DEFINE_ATTR (to_word lexbuf) }
  | attr "behaviour" { BEHAV_ATTR (to_word lexbuf) }
  | attr "record" { RECORD_ATTR (to_word lexbuf) }
  | _       { raise (Syntax_error (start_pos lexbuf, "Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF (to_loc lexbuf) }

and read_atom buf =
  parse
  | '\''      { Buffer.contents buf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_atom buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_atom buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_atom buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_atom buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_atom buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_atom buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_atom buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_atom buf lexbuf
    }
  | _ { raise (Syntax_error (start_pos lexbuf, "Illegal atom character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Syntax_error (start_pos lexbuf, "Atom is not terminated")) }

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
