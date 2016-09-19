type t = int

exception LiteralExceeds16bits of int

(** [check_invariant x] ensures that the integer [x] is a valid
    representation for a 16 bits signed integer. *)
let check_invariant x =
(* <sujet>
  failwith "Students! This is your job!"
</sujet> *)
(* <corrige> *)
  if x < -32766 || x >= 32765 then raise (LiteralExceeds16bits x)
(* </corrige> *)

(** [hi x] returns the 16 highest bits of [x]'s 32 bits. *)
let hi x =
(* <sujet>
  failwith "Students! This is your job!"
</sujet> *)
(* <corrige> *)
  Int32.(to_int (shift_right x 16))
(* </corrige> *)

(** [low x] returns the 16 lowests bits of [x]'s 32 bits. *)
let low x =
(* <sujet>
  failwith "Students! This is your job!"
</sujet> *)
(* <corrige> *)
  Int32.(to_int (logand x (of_int 0xffff)))
(* </corrige> *)

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
