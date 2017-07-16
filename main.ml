open Core.Std
open Printf

let usage = "usage: ei <command> [parameters] [options]"

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
            let file =
              In_channel.create file
              |> In_channel.input_all
              |> File.create
            in
            let node = Parser.prog Lexer.read buf in
            f file node
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

let compare_texts before after =
  printf "--------------------------------------------------------------\n";
  printf "Compare difference before and after formatted texts: ";
  let strip s =
    String.filter before ~f:(fun c -> Char.is_whitespace c |> not)
  in
  if strip before = strip after then
    printf "ok\n"
  else begin
    printf "failed\n";
    exit (-1)
  end

let main =
  Command.basic
    ~summary:usage
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> flag "-syntax" no_arg ~doc:" check syntax only"
      +> flag "-test" no_arg ~doc:" check difference before and after formatted texts"
      +> anon (maybe ("filename" %: string))
    )
    (fun debug verbose syntax test file () ->
       Conf.debug_mode := debug;
       Conf.verbose_mode := verbose;
       parse_file file
         ~f:(fun file node ->
             if not syntax then begin
               let before, after = Formatter.format file node in
               printf "%s\n" after;
               if test then begin
                 compare_texts before after
               end
             end))

let () =
  try
    Printexc.record_backtrace true;
    Command.run
      ~version:(sprintf "ei %s" Conf.version)
      main
  with
  | e -> raise e
