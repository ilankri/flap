(** This module defines a memory model. *)

(** A memory is data structure... *)
type 'a t

(** that maps locations... *)
type location

(** to blocks of data of type ['a]. *)
type 'a block

(** [create size] produces a fresh memory of [size] potential blocks. *)
val create : int -> 'a t

(** [allocate mem size init] produces a location that points to a fresh block
    of size cells. These cells are initialized with [init]. *)
val allocate : 'a t -> int -> 'a -> location

(** The following exception is raised if no new block can be allocated in the
    memory. *)
exception OutOfMemory

(** [dereference mem location] returns the block pointed by [location]. *)
val dereference : 'a t -> location -> 'a block

(** [read block i] returns the content of the i-th cell of the block *)
val read : 'a block -> int -> 'a

(** [write block i x] sets the content of the i-th cell of the block to [x]. *)
val write : 'a block -> int -> 'a -> unit

(** [set_tag block t] sets the tag of [block] to [t]. *)
val set_tag : 'a block -> int -> unit

(** [get_tag block] returns the tag of [block]. *)
val get_tag : 'a block -> int

(** [print_location l] returns a human-readable representation of [l]. *)
val print_location : location -> string
