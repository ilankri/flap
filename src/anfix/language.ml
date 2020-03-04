(** The anfix programming language. *)

module Ast = Ast

type ast = Ast.t

let name = "anfix"

let parse lexer_init input =
  FromFopix.program (Fopix.parse lexer_init input)

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".anfix"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  let ast' = ToFopix.program ast in
  Fopix.PrettyPrinter.(to_string program ast')

(* The interpretor is the one of Fopix *)
include Fopix.Interpreter
let evaluate ast = Fopix.Interpreter.evaluate (ToFopix.program ast)

(* No typechecking for Anfix *)
type typing_environment = unit
let initial_typing_environment () = ()
let typecheck () _ = ()
let print_typing_environment () = ""
