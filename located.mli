type 'a t = {
  desc : 'a;
  loc : Location.t option;
}

val create : Location.t option -> 'a -> 'a t
val with_range : Location.t -> Location.t -> 'a -> 'a t
