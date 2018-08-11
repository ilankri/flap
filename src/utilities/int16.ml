type t = int

exception LiteralExceeds16bits of int

(** [check_invariant x] ensures that the integer [x] is a valid
    representation for a 16 bits signed integer. *)
let check_invariant x =
  let max_int16 = 1 lsl 15 - 1 in
  let min_int16 = -(max_int16 + 1) in
  if x < min_int16 || x > max_int16 then raise (LiteralExceeds16bits x)

(** [hi x] returns the 16 highest bits of [x]'s 32 bits. *)
let hi x =
  failwith "Students! This is your job!"

(** [low x] returns the 16 lowests bits of [x]'s 32 bits. *)
let low x =
  failwith "Students! This is your job!"

(** [of_int x] turns an OCaml integer literal into a 16 bits literal. *)
let of_int x =
  check_invariant x;
  x

(** [of_int32 x] turns an OCaml integer literal into a 16 bits literal. *)
let of_int32 x =
  of_int (Int32.to_int x)

(** [to_string x] turns an integer [x] into a string. *)
let to_string x =
  string_of_int x
