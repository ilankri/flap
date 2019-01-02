(** The anfix programming language. *)

module AST = AnfixAST

type ast = AnfixAST.t

let name = "anfix"

let parse lexer_init input =
  FopixToAnfix.program (Fopix.parse lexer_init input)

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".anfix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  let ast' = AnfixToFopix.program ast in
  FopixPrettyPrinter.(to_string program ast')

(* The interpretor is the one of Fopix *)
include FopixInterpreter
let evaluate r ast = FopixInterpreter.evaluate r (AnfixToFopix.program ast)

(* No typechecking for Anfix *)
type typing_environment = unit
let initial_typing_environment () = ()
let typecheck () _ = ()
let print_typing_environment () = ""
