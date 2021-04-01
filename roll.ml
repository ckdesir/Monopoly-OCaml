type roll = int

(** [dice ()] returns a random integer between 1 and 6 *)
let dice () =
  Random.self_init ();
  1 + Random.int 6
