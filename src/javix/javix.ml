(** The javix programming language. *)

module AST = JavixAST

type ast =
    JavixAST.t

let name = "javix"

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:JavixLexer.token
    ~parser_fun:JavixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".j"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  let ast' =
    { ast with AST.classname = !Options.compilation_unit_name }
  in
  JavixPrettyPrinter.(to_string program ast')

include JavixInterpreter
include JavixTypechecker
