(** The retrolix programming language. *)

module AST = RetrolixAST

let name = "retrolix"

type ast =
    RetrolixAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:RetrolixLexer.token
    ~parser_fun:RetrolixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".retrolix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  RetrolixPrettyPrinter.(to_string program ast)

include RetrolixInterpreter
include RetrolixTypechecker
