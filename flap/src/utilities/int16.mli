(** This module implements 16 bits signed integers. *)

type t

(** This exception is raised if a literal is too large to be
    represented using only 16 bits. *)
exception LiteralExceeds16bits of int

(** [of_int32 x] turns an OCaml integer literal into a 16 bits literal. *)
val of_int32 : Int32.t -> t

(** [of_int x] turns an OCaml integer literal into a 16 bits literal. *)
val of_int : int -> t

(** [hi x] returns the 16 highests bits of [x]'s 32 bits. *)
val hi : Int32.t -> t

(** [low x] returns the 16 lowests bits of [x]'s 32 bits. *)
val low : Int32.t -> t

(** [to_string x] turns an integer [x] into a string. *)
val to_string : t -> string
