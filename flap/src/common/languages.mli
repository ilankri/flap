module type Language = sig

  (** A language as a [name]. *)
  val name : string

  (** {1 Syntax} *)

  (** A syntax is defined by the type of abstract syntax trees. *)
  type ast

  (** [parse_filename f] turns the content of file [f] into an
      abstract syntax tree if that content is a syntactically valid
      input. *)
  val parse_filename : string -> ast

  (** Each language has its own extension for source code filenames. *)
  val extension : string

  (** [parse_string c] is the same as [parse_filename] except that the
      source code is directly given as a string. *)
  val parse_string : string -> ast

  (** [print ast] turns an abstract syntax tree into a human-readable
      form. *)
  val print_ast : ast -> string

  (** {2 Dynamic semantic} *)

  (** A runtime environment contains all the information necessary
      to evaluate a program. *)
  type runtime

  (** In the interactive loop, we will display some observable
      feedback about the evaluation. *)
  type observable

  (** The evaluation starts with an initial runtime. *)
  val initial_runtime : unit -> runtime

  (** [evaluate runtime p] executes the program [p] and
      produces a new runtime as well as an observation
      of this runtime. *)
  val evaluate : runtime -> ast -> runtime * observable

  (** [print_observable o] returns a human-readable
      representation of an observable. *)
  val print_observable : runtime -> observable -> string

  (** {3 Static semantic} *)

  (** A typing environment stores static information about the program. *)
  type typing_environment

  (** The initial typing environment contains predefined static information,
      like the type for constants. *)
  val initial_typing_environment : unit -> typing_environment

  (** [typecheck env p] checks if the program [p] is well-formed
      and enriches the typing environment accordingly. If [p] is
      not well-formed an {!Error} is issued. *)
  val typecheck : typing_environment -> ast -> typing_environment

  (** [print_typing_environment] returns a human-readable
      representation of a typing environment. *)
  val print_typing_environment : typing_environment -> string

end

(** [get name] returns a language of flap called [name] if it exists. *)
val get : string -> (module Language)

(** [register l] inserts [l] in the set of flap's languages. *)
val register : (module Language) -> unit
