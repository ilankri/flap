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
    located literal l

  | Variable x ->
    located identifier x

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 80 b (f x);
  Buffer.contents b
