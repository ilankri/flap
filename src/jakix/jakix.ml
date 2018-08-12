(** The Jakix programming language :
    same as Javix, but for compiling CPS-language (Kontix) *)

(** We reuse the same AST as Javix, and printer, parser, etc *)
module JakixAST = JavixAST
module JakixPrettyPrinter = JavixPrettyPrinter
module JakixLexer = JavixLexer
module JakixParser = JavixParser
module JakixInterpreter = JavixInterpreter
module JakixTypechecker = JavixTypechecker

module AST = JakixAST

type ast = JakixAST.t

let name = "jakix"

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:JakixLexer.token
    ~parser_fun:JakixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".k"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  let ast' =
    { ast with AST.classname = !Options.compilation_unit_name }
  in
  JakixPrettyPrinter.(to_string program ast')

include JakixInterpreter
include JakixTypechecker
