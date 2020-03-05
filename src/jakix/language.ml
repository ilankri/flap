(** The Jakix programming language :
    same as Javix, but for compiling CPS-language (Kontix) *)

(** We reuse the same AST as Javix, and printer, parser, etc *)
module Ast = Javix.Ast
module PrettyPrinter = Javix.PrettyPrinter
module Lexer = Javix.Lexer
module Interpreter = Javix.Interpreter
module Typechecker = Javix.Typechecker

let name = "jakix"

let extension =
  ".k"

module Parser = struct
  type ast = Ast.t

  let parse lexer_init input =
    Util.SyntacticAnalysis.process
      ~lexer_init
      ~lexer_fun:Lexer.token
      ~parser_fun:Javix.Parser.program
      ~input

  let parse_filename filename =
    parse Lexing.from_channel (open_in filename)

  let parse_string =
    parse Lexing.from_string

  let print_ast ast =
    let ast' =
      { ast with Ast.classname = !Options.compilation_unit_name }
    in
    PrettyPrinter.(to_string program ast')
end
