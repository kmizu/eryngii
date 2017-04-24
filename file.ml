open Core.Std

type t = {
  contents : string;
  length : int;
  lines : string array;
  line_offsets : int list;
}

let create contents =
  let length = String.length contents in
  let lines = String.split_lines contents |> Array.of_list in
  let line_offsets = String.foldi contents
      ~init:[0]
      ~f:(fun i lines c ->
          match c with
          | '\r' -> i :: lines
          | '\n' ->
            if i > 0 then begin
              match String.get contents (i-1) with
              | '\r' -> lines
              | _ -> i :: lines
            end else
              i :: lines
          | _ -> lines)
  in
  { contents; length; lines; line_offsets }
