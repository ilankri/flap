(** Register some compilers that have MIPS as a target or source language. *)
let initialize () =
  Languages.register (module Mips);
  Compilers.register (module RetrolixToMips)
