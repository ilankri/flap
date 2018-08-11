let initialize () =
  Languages.register (module Fopix);
  Compilers.register (module Compilers.Identity (Fopix));
  Compilers.register (module HobixToFopix)
