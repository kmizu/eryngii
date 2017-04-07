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

let lowercase s =
  String.foldi s
    ~init:[]
    ~f:(fun i accu c ->
        let c' = Char.lowercase c in
        match pre_case s i, case c, next_case s i with
        | Some `Upper, `Upper, Some `Lower
        | Some `Lower, `Upper, _ ->
          c' :: '_' :: accu
        | _ -> c' :: accu)
  |> List.rev
  |> String.of_char_list

let test () =
  Printf.printf "lowercase: %s\n" (lowercase "HTTP");
  Printf.printf "lowercase: %s\n" (lowercase "HTTPClient");
  Printf.printf "lowercase: %s\n" (lowercase "ClientA");
  Printf.printf "lowercase: %s\n" (lowercase "CamelCaseName");
;;

test ()
