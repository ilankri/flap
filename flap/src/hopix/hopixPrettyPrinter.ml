open PPrint
open PPrintCombinators
open PPrintEngine
open ExtPPrint
open HopixAST
open Position
open Literal

let rec program p =
  separate_map hardline (located definition) p

and definition = function
  | DefineValue (x, e) ->
    group (value_definition "val" (x, e))

and identifier (Id x) =
  string x

and value_definition what (x, e) =
  nest 2 (group (group (string what ++ located identifier x ++ string "=")
		 ++ group (located expression e)))

and expression = function
  | Literal l ->
    literal l

  | Variable x ->
    identifier x

  | Apply (f, args) ->
    nest 2 (may_paren_expression (Position.value f) `AtLeftOfApplication
	    ++ tuple args)

and expression' e =
  expression (Position.value e)

and may_paren_expression what where =
  match what, where with
    | (Variable _, _) -> expression what
    | (_, `AtLeftOfApplication) -> parens (expression what)

and tuple es =
  parens (separate_map (comma ^^ break 1) expression' es)


let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 80 b (f x);
  Buffer.contents b
