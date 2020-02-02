let initialize () =
  Common.Languages.register (module Language);
  Common.Compilers.register
    (module Common.Compilers.Identity (Language) : Common.Compilers.Compiler);
  Common.Compilers.register (module FromKontix : Common.Compilers.Compiler)
