(** The Hopix programming language. *)

let name = "hopix"

module Ast = Ast

type ast = Ast.t

let parse lexer_init input =
  Util.SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:Lexer.token
    ~parser_fun:Parser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".hopix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  PrettyPrinter.(to_string program ast)

let print_expression e =
  PrettyPrinter.(to_string expression e)

include Interpreter

include Typechecker
