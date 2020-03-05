(** The kontix programming language :
    similar to fopix, but with tail calls only *)

module Ast = Ast

let name = "kontix"

let extension =
  ".kontix"

module Parser = struct
  type ast = Ast.t

  let parse lexer_init input =
    Util.SyntacticAnalysis.process
      ~lexer_init
      ~lexer_fun:Lexer.token
      ~parser_fun:Parser.program
      ~input

  let parse_filename filename =
    parse Lexing.from_channel (open_in filename)

  let parse_string = parse Lexing.from_string

  let print_ast ast =
    PrettyPrinter.(to_string program ast)
end

(* For the moment, the interpretor is the one of Fopix *)
module Interpreter = struct
  include Fopix.Interpreter
  let evaluate ast = Fopix.Interpreter.evaluate (ToFopix.program ast)
end

(* No typechecking for Kontix *)
module Typechecker = struct
  type typing_environment = unit
  let initial_typing_environment () = ()
  let typecheck () _ = ()
  let print_typing_environment () = ""
end
