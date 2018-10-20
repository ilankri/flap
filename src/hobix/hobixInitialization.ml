let initialize () =
  Languages.register (module Hobix);
  Compilers.register (module Compilers.Identity (Hobix));
  Compilers.register (module HopixToHobix)
