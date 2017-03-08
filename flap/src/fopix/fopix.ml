(** The fopix programming language. *)

module AST = FopixAST

let name = "fopix"

type ast = FopixAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:FopixLexer.token
    ~parser_fun:FopixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".fopix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  FopixPrettyPrinter.(to_string program ast)

include FopixInterpreter
include FopixTypechecker
