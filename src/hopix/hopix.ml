(** The Hopix programming language. *)

let name = "hopix"

module AST = HopixAST

type ast = HopixAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:HopixLexer.token
    ~parser_fun:HopixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".hopix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  HopixPrettyPrinter.(to_string program ast)

let print_expression e =
  HopixPrettyPrinter.(to_string expression e)

include HopixInterpreter

include HopixTypechecker
