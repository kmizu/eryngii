open Core.Std
open Printf

let file_exists file =
  match Sys.file_exists ~follow_symlinks:true file with
  | `No | `Unknown ->
    Printf.printf "Error: No such file `%s'\n" file;
    exit (-1)
  | `Yes -> ()

let parse_file file =
  file_exists file;
  In_channel.with_file file
    ~f:(fun chan ->
          let buf = Lexing.from_channel chan in
          try begin
            Parser.prog Lexer.read buf
          end with
          | Lexer.Syntax_error (pos, msg) ->
            let open Position in
            printf "Line %d, column %d: Invalid syntax: %s\n" pos.line pos.col msg;
            exit (-1)
          | Parser.Error ->
            let pos = Lexing.lexeme_start_p buf in
            printf "Line %d, column %d: Invalid syntax\n"
              pos.pos_lnum (pos.pos_cnum+1);
            exit (-1)
          | e -> raise e)

let command =
  Command.basic
    ~summary: (sprintf "Erlang lint, version %s" Conf.version)
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> flag "-syntax" no_arg ~doc:" check syntax only"
      +> flag "-debug-ast" no_arg ~doc:" print parse tree"
      +> anon (maybe ("filename" %: string))
    )
    (fun debug verbose syntax debug_ast file_opt () ->
      try
        Printexc.record_backtrace true;
        match file_opt with
        | Some file ->
          if syntax then
            ignore @@ parse_file file
          else if debug_ast then
            match parse_file file with
            | Some node -> printf "%s" (Ast.to_string node)
            | None -> printf "fail parse"
          else
            () (* TODO *)
        | None ->
          Printf.printf "Error: No input files";
          exit 1
      with
      | e -> raise e)

let () =
  Command.run ~version:Conf.version command
