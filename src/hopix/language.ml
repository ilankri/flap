(** The Hopix programming language. *)

let name = "hopix"

let extension =
  ".hopix"

module Ast = Ast

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

  let parse_string =
    parse Lexing.from_string

  let print_ast ast =
    PrettyPrinter.(to_string program ast)
end

let print_expression e =
  PrettyPrinter.(to_string expression e)

module Interpreter = Interpreter

module Typechecker = Typechecker
