(** Compilers

    A compiler is a translator from a source language to a target
    language.

*)
open Languages

module type Compiler = sig

  module Source : Language
  module Target : Language

  (** It is convenient to maintain some information about a program
      along its compilation: an environment is meant to store that
      kind of information. *)
  type environment
  val initial_environment : unit -> environment

  (** [translate source env] returns a [target] program semantically
      equivalent to [source] as a well as an enriched environment
      [env] that contains information related to the compilation of
      [source]. *)
  val translate : Source.ast -> environment -> Target.ast * environment

end

(** [register compiler] integrates [compiler] is the set of flap's compilers. *)
val register : (module Compiler) -> unit

(** [get ?using source target] returns a compiler from [source] to
    [target] built by composing flap's compilers. [using] is empty if
    not specified.

    [using] represents a list of languages that must appear in the
    compilation chain. It is useful to disambiguate between several
    choices when distinct compilation chains exist between two
    languages. If [using] is not precise enough to kill the
    ambiguity, flap issues a global error. *)
val get : ?using:(module Language) list
  -> (module Language) -> (module Language) -> (module Compiler)

(** There is an easy way to compile a language into itself:
    just use the identity function :-). *)
module Identity (L : Language) : Compiler
