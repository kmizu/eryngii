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
            let lines =
              In_channel.create file
              |> In_channel.input_all
              |> String.split_lines
            in
            Annot.init @@ List.length lines;
            let node = Parser.prog Lexer.read buf in
            f lines node
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
         ~f:(fun lines node ->
             let fmt = Formatter.format lines node in
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
       parse_file file ~f:(fun _ _ -> ()))

let test_format =
  Command.basic
    ~summary: "test formatter"
    Command.Spec.(
      empty
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> anon ("expected" %: string)
      +> anon ("actual" %: string)
    )
    (fun verbose expected actual () ->
       Conf.verbose_mode := verbose;
       parse_file (Some actual)
         ~f:(fun lines node ->
             let rec compare i ex_lines ac_lines =
               match ex_lines, ac_lines with
               | [], _
               | _, [] ->
                 printf "Error: two formatted files have different number of lines\n";
                 exit 1
               | ex :: ex_lines, ac :: ac_lines ->
                 if verbose then begin
                   printf "Line %d:\n" i;
                   printf "    Expected -> \"%s\"\n" ex;
                   printf "    Actual   -> \"%s\"\n" ac;
                   printf "\n";
                 end;
                 if ex = ac then
                   compare (i+1) ex_lines ac_lines
                 else begin
                   printf "Error: line %d:\n" i;
                   printf "    Expected -> \"%s\"\n" ex;
                   printf "    Actual   -> \"%s\"\n" ac;
                   exit 1
                 end
             in
             let ac = Formatter.format lines node |> String.split_lines in
             let ex =
               In_channel.create expected
               |> In_channel.input_all
               |> String.split_lines
             in
             compare 0 ex ac))

let main =
  Command.group
    ~summary:usage
    [
      ("fmt", fmt);
      ("syntax", syntax);
      ("testfmt", test_format);
    ]

let () =
  try
    Printexc.record_backtrace true;
    Command.run
      ~version:(sprintf "eryngii %s" Conf.version)
      main
  with
  | e -> raise e
