(** Register some compilers that have MIPS as a target or source language. *)
let initialize () =
  Common.Languages.register (module Language);
  Common.Compilers.register (module FromRetrolix)
