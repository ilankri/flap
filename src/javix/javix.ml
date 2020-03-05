module Lexer = Lexer
module PrettyPrinter = PrettyPrinter
module FromFopix = FromFopix

include Language

(** Register some compilers that have Javix as a target or source language. *)
let initialize () =
  Common.Languages.register (module Language);
  Common.Compilers.register
    (module Common.Compilers.Identity (Language) : Common.Compilers.Compiler);
  Common.Compilers.register (module FromFopix : Common.Compilers.Compiler)
