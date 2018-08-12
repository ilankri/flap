(** The kontix programming language :
    similar to fopix, but with tail calls only *)

module AST = KontixAST

type ast = KontixAST.t

let name = "kontix"

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:KontixLexer.token
    ~parser_fun:KontixParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".kontix"

let parse_string = parse Lexing.from_string

let print_ast ast =
  KontixPrettyPrinter.(to_string program ast)

(* For the moment, the interpretor is the one of Fopix *)
include FopixInterpreter
let evaluate r ast = FopixInterpreter.evaluate r (KontixToFopix.program ast)

(* No typechecking for Kontix *)
type typing_environment = unit
let initial_typing_environment () = ()
let typecheck () ast = ()
let print_typing_environment () = ""
