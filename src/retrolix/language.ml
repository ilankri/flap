(** The retrolix programming language. *)

module Ast = Ast

let name = "retrolix"

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

let extension =
  ".retrolix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  PrettyPrinter.(to_string program ast)

include Interpreter
include Typechecker
