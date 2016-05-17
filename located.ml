type 'a t = {
  desc : 'a;
  loc : Location.t option;
}

let create loc desc =
  { desc; loc }

let with_range start_loc end_loc desc =
  create (Some (Location.union start_loc end_loc)) desc
