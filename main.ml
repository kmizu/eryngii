open Core.Std
open Printf

let usage = "usage: eryngii <command> [parameters] [options]"

let file_exists file =
  match Sys.file_exists ~follow_symlinks:true file with
  | `No | `Unknown ->
    Printf.printf "Error: No such file `%s'\n" file;
    exit (-1)
  | `Yes -> ()

let parse_file file ~f =
  match file with
  | None ->
    Printf.printf "Error: No input files";
    exit 1
  | Some file ->
    file_exists file;
    In_channel.with_file file
      ~f:(fun chan ->
          let buf = Lexing.from_channel chan in
          try begin
            f @@ Parser.prog Lexer.read buf
          end with
          | Lexer.Syntax_error (pos, msg) ->
            let open Position in
            printf "Line %d, column %d: %s\n" pos.line pos.col msg;
            exit (-1)
          | Parser.Error ->
            let pos = Lexing.lexeme_start_p buf in
            printf "Line %d, column %d: Invalid syntax\n"
              pos.pos_lnum (pos.pos_cnum-pos.pos_bol+1);
            exit (-1)
          | e -> raise e)

let run file ~f =
  match file with
  | Some file ->
    f @@ parse_file file
  | None ->
    Printf.printf "Error: No input files";
    exit 1

let fmt =
  Command.basic
    ~summary: "formats a source file"
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> anon (maybe ("filename" %: string))
    )
    (fun debug verbose file () ->
       parse_file file
         ~f:(fun node ->
             let fmt = Formatter.format node in
             printf "%s\n" fmt))

let syntax =
  Command.basic
    ~summary: "check syntax of a source file"
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> anon (maybe ("filename" %: string))
    )
    (fun debug verbose file () ->
       parse_file file ~f:ignore)

let main =
  Command.group
    ~summary:usage
    [
      ("fmt", fmt);
      ("syntax", syntax);
    ]

let () =
  try
    Printexc.record_backtrace true;
    Command.run
      ~version:(sprintf "eryngii %s" Conf.version)
      main
  with
  | e -> raise e
