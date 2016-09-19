(** The abstract syntax tree for hopix programs. *)

open Position

(** A program is a list of definitions. *)
type program = (definition located) list

and definition =
  (** A toplevel definition for a value.

      In concrete syntax

                         val x = 2

      is

      DefineValue ({ value = Id "x"; position = ... },
                   { value = Literal { value = Int32 2; position = ... }; position = ... })


      In concrete syntax

                         val x = y

      DefineValue ({ value = Id "x"; position = ... },
                   { value = Variable { value = Id "y" ; position = ... }; position = ... })

  *)
  | DefineValue of identifier located * expression located

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal located
  (** A variable identifies a value. *)
  | Variable of identifier located

and literal = Literal.t

and identifier = Id of string

and t = program
