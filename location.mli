type t = {
  start : Position.t;
  end_ : Position.t;
  len : int;
}

val zero : t

val create : Position.t -> Position.t -> t

val union : t -> t -> t

val contains_pos : t -> Position.t -> bool
val contains_offset : t -> int -> bool
