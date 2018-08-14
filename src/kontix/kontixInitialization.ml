let initialize () =
  Languages.register (module Kontix);
  Compilers.register (module Compilers.Identity (Kontix) : Compilers.Compiler);
  Compilers.register (module AnfixToKontix : Compilers.Compiler)
