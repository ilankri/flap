include Language

let initialize () =
  Common.Languages.register (module Language);
  Common.Compilers.register (module Common.Compilers.Identity (Language));
  Common.Compilers.register (module FromHopix)
