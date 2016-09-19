open PPrint
open PPrintCombinators
open PPrintEngine
open ExtPPrint

type t =
  | LInt    of Int32.t

let int i =
  string (Int32.to_string i)

let literal = function
  | LInt x ->
    int x
