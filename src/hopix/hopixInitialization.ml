let initialize () =
  Languages.register (module Hopix);
  Compilers.register (module Compilers.Identity (Hopix))

