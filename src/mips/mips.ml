(** The MIPS programming language. *)

module AST = MipsAST

let name =
  "mips"

type ast = MipsAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:MipsLexer.token
    ~parser_fun:MipsParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".mips"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  MipsPrettyPrinter.(to_string program ast)

include MipsInterpreter
include MipsTypechecker
