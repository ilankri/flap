module Language = Language

module RealFopixToAnfix = struct
  module Source=Fopix
  module Target = Language
  type environment = unit
  let initial_environment () = ()
  let translate ast () = FromFopix.program ast, ()
end

let initialize () =
  Common.Languages.register (module Language);
  Common.Compilers.register
    (module Common.Compilers.Identity (Language) : Common.Compilers.Compiler);
  Common.Compilers.register
    (module RealFopixToAnfix : Common.Compilers.Compiler)
