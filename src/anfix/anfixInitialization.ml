module RealFopixToAnfix = struct
  module Source=Fopix
  module Target=Anfix
  type environment = unit
  let initial_environment () = ()
  let translate ast () = FopixToAnfix.program ast, ()
end

let initialize () =
  Compilers.register (module Compilers.Identity (Anfix) : Compilers.Compiler);
  Compilers.register (module RealFopixToAnfix : Compilers.Compiler)
