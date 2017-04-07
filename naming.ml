open Core.Std

let get_opt s i = 
  if i <= 0 || +1 >= String.length s then
    None
  else
    Some (String.get s i)

let case c =
  if Char.is_uppercase c then `Upper else `Lower

let pre_case s i =
  Option.value_map (get_opt s (i-1))
    ~default:None
    ~f:(fun c -> Some (case c))

let next_case s i =
  if i+1 >= String.length s then None else Some (case @@ String.get s (i+1))

let convert_case s ~f =
  String.foldi s
    ~init:[]
    ~f:(fun i accu c ->
        let c' = f c in
        match pre_case s i, case c, next_case s i with
        | Some `Upper, `Upper, Some `Lower
        | Some `Lower, `Upper, _ ->
          c' :: '_' :: accu
        | _ -> c' :: accu)
  |> List.rev
  |> String.of_char_list

let lowercase s = convert_case s ~f:Char.lowercase

let uppercase s = convert_case s ~f:Char.uppercase

let test () =
  Printf.printf "lowercase: HTTP -> %s\n" (lowercase "HTTP");
  Printf.printf "lowercase: HTTPClient -> %s\n" (lowercase "HTTPClient");
  Printf.printf "lowercase: ClientA -> %s\n" (lowercase "ClientA");
  Printf.printf "lowercase: CamelCaseName -> %s\n" (lowercase "CamelCaseName");
  Printf.printf "uppercase: HTTP -> %s\n" (uppercase "HTTP");
  Printf.printf "uppercase: HTTPClient -> %s\n" (uppercase "HTTPClient");
  Printf.printf "uppercase: ClientA -> %s\n" (uppercase "ClientA");
  Printf.printf "uppercase: CamelCaseName -> %s\n" (uppercase "CamelCaseName");
;;

(* test () *)
