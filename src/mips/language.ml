(** The MIPS programming language. *)

module Ast = Ast

let name =
  "mips"

let extension =
  ".mips"

module Parser = struct
  type ast = Ast.t

  let parse lexer_init input =
    let fail_no_mips what = failwith @@ Format.sprintf "No %s for MIPS" what in
    Util.SyntacticAnalysis.process
      ~lexer_init
      ~lexer_fun:(fun _ -> fail_no_mips "lexer")
      ~parser_fun:(fun _ _ -> fail_no_mips "parser")
      ~input

  let parse_filename filename =
    parse Lexing.from_channel (open_in filename)

  let parse_string =
    parse Lexing.from_string

  let print_ast ast =
    PrettyPrinter.(to_string program ast)
end

module Interpreter = Interpreter

module Typechecker = Typechecker
