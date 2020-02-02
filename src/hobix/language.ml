(** The hobix programming language. *)

let name = "hobix"

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
  ".hobix"

let parse_string s =
  parse Lexing.from_string s

let print_ast ast =
  PrettyPrinter.(to_string program ast)

let print_expression e =
  PrettyPrinter.(to_string expression e)

include Interpreter

include Typechecker
