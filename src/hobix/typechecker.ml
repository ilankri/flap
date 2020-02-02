(** This module implements a type checker for Datix. *)

let initial_typing_environment = Types.initial_typing_environment

type typing_environment = Types.typing_environment

let typecheck tenv _ = tenv

let print_typing_environment =
  Types.print_typing_environment
