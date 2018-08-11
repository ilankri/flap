(** The hobix programming language. *)

let name = "hobix"

module AST = HobixAST

type ast = HobixAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:HobixLexer.token
    ~parser_fun:HobixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".hobix"

let parse_string s =
  parse Lexing.from_string s

let print_ast ast =
  HobixPrettyPrinter.(to_string program ast)

let print_expression e =
  HobixPrettyPrinter.(to_string expression e)

include HobixInterpreter

include HobixTypechecker
