(** The javix programming language. *)

module Ast = Ast

let name = "javix"

let extension =
  ".j"

module Parser = struct
  type ast =
    Ast.t

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
    let ast' =
      { ast with Ast.classname = !Options.compilation_unit_name }
    in
    PrettyPrinter.(to_string program ast')

  let program lexer buffer = Parser.program lexer buffer
end

module Interpreter = Interpreter

module Typechecker = Typechecker
