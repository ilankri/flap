(** The anfix programming language. *)

module Ast = Ast

let name = "anfix"

let extension =
  ".anfix"

module Parser = struct
  type ast = Ast.t

  let parse lexer_init input =
    FromFopix.program (Fopix.Parser.parse lexer_init input)

  let parse_filename filename =
    parse Lexing.from_channel (open_in filename)

  let parse_string =
    parse Lexing.from_string

  let print_ast ast =
    let ast' = ToFopix.program ast in
    Fopix.PrettyPrinter.(to_string program ast')
end

(* The interpretor is the one of Fopix *)
module Interpreter = struct
  include Fopix.Interpreter
  let evaluate ast = evaluate (ToFopix.program ast)
end

(* No typechecking for Anfix *)
module Typechecker = struct
  type typing_environment = unit
  let initial_typing_environment () = ()
  let typecheck () _ = ()
  let print_typing_environment () = ""
end
