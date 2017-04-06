open Core.Std

type ('a, 'b) t =
  | Cons of 'a * ('b, 'a) t
  | Nil

let empty = Nil

let one e = Cons (e, Nil)

let cons e ~sep es = Cons (e, Cons (sep, es))

let rev es =
  let rec f es accu =
    match es with
    | Nil -> Nil
    | Cons (e, Nil) -> Cons (e, accu)
    | Cons (_e, Cons (_sep, Nil)) -> failwith "error"
    | Cons (e, Cons (sep, es)) ->
      f es @@ Cons (sep, Cons (e, accu))
  in
  f es Nil

let fold_left es ~init ~f =
  let rec fold_left' es accu =
    match es with
    | Nil -> accu
    | Cons (e, Nil) -> f accu None e
    | Cons (_e, Cons (_sep, Nil)) -> failwith "error"
    | Cons (e, Cons (sep, es)) ->
      fold_left' es @@ f accu (Some sep) e
  in
  fold_left' es init

let length es =
  fold_left es ~init:0 ~f:(fun accu _sep _e -> accu + 1)

let iter es ~f =
  fold_left es ~init:() ~f:(fun _accu sep e -> f sep e)

let opt_iter es_opt ~f =
  Option.iter es_opt ~f:(fun es -> iter es ~f)
