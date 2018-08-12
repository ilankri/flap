(** Register some compilers that have Javix as a target or source language. *)
let initialize () =
  Compilers.register (module Compilers.Identity (Javix) : Compilers.Compiler);
  Compilers.register (module FopixToJavix : Compilers.Compiler)
