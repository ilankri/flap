let initialize () =
  Languages.register (module Jakix);
  Compilers.register (module Compilers.Identity (Jakix) : Compilers.Compiler);
  Compilers.register (module KontixToJakix : Compilers.Compiler)
