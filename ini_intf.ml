module type Ini = sig

  type t = Ini_type.t

  type parse_error = Ini_type.parse_error =
    { pos : Lexing.position; error : string; }

  type parse_result =
    | Success of t
    | Failure of parse_error

  val read : string -> parse_result

  val sections : t -> string list
  val mem_section : t -> string -> bool
  val options : t -> string -> (string * string) list option
  val mem_option : t -> string -> string -> bool
  val get : t -> string -> string -> string option
  val iter : t -> string -> (string -> string -> unit) -> unit

  val value_to_list : ?on:char -> string -> string list
  (** 文字列を on の文字で区切ったリストを返す *)

  module Parser : sig
    type token
    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t
  end

  module Lexer : sig
    val main : Lexing.lexbuf -> Parser.token
    val syntax_error : Lexing.lexbuf -> string -> parse_error
  end

end

