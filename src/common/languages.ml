module type Language = sig

  (** A language as a [name]. *)
  val name : string

  (** Each language has its own extension for source code filenames. *)
  val extension : string

  (** {1 Syntax} *)
  module Parser : sig
    (** A syntax is defined by the type of abstract syntax trees. *)
    type ast

    (** [parse_filename f] turns the content of file [f] into an
        abstract syntax tree if that content is a syntactically valid
        input. *)
    val parse_filename : string -> ast

    (** [parse_string c] is the same as [parse_filename] except that the
        source code is directly given as a string. *)
    val parse_string : string -> ast

    (** [print ast] turns an abstract syntax tree into a human-readable
        form. *)
    val print_ast : ast -> string
  end

  (** {2 Semantic} *)
  module Interpreter : sig
    (** A runtime environment contains all the information necessary
        to evaluate a program. *)
    type runtime

    (** In the interactive loop, we will display some observable
        feedback about the evaluation. *)
    type observable

    (** The evaluation starts with an initial runtime. *)
    val initial_runtime : unit -> runtime

    module Machine : Util.StateMonad.S with type state = runtime

    (** [evaluate p] executes the program [p] and
        produces a new runtime as well as an observation
        of this runtime. *)
    val evaluate : Parser.ast -> observable Machine.t

    (** [print_observable o] returns a human-readable
        representation of an observable. *)
    val print_observable : runtime -> observable -> string
  end

  (** {3 Static semantic} *)
  module Typechecker : sig
    (** During type checking, static information (aka types)
        are stored in the typing environment. *)
    type typing_environment

    (** A typing environment to start with. *)
    val initial_typing_environment : unit -> typing_environment

    (** [typecheck tenv p] checks if [p] is a well-typed program
        and returns an extension of the typing environment [tenv]
        with the values defined in the program. *)
    val typecheck : typing_environment -> Parser.ast -> typing_environment

    (** [print_typing_environment tenv] returns a human-readable
        representation of [tenv]. *)
    val print_typing_environment : typing_environment -> string
  end
end

(** We store all the language implementations in the following
    hashing table. *)
let languages : (string, (module Language)) Hashtbl.t =
  Hashtbl.create 13

let get (l : string) : (module Language) =
  try
    Hashtbl.find languages l
  with Not_found ->
    Util.Error.global_error "initialization" "There is no such language."

let register (module L : Language) =
  Hashtbl.add languages L.name (module L)
