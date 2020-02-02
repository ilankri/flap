open PPrintCombinators
open PPrintEngine
open Util.ExtPPrint
open Ast

let int i = string (Int32.to_string i)

let gtype_definition sep what ks =
  string ":=" ++ group (
    string "{"
    ++ separate_map (break 1 ^^ string sep ^^ break 1) what ks
    ++ string "}"
  )

let rec program p =
  separate_map hardline (definition) p

and definition = function
  | DefineValue (x, e) ->
      group (value_definition "val" (x, e) ^^ string ".")
  | DefineRecFuns rv ->
      group
        (string "fun"
         ++ separate_map (hardline ^^ string "and") (value_definition "") rv)
      ^^ string "."
  | DeclareExtern x ->
      group (string "extern" ++ identifier x)

and value_definition what (x, e) =
  nest 2 (group (group (string what ++ identifier x ++ string ":=")
                 ++ group (expression e)))

and identifier (Id x) =
  string x

and expression = function
  | Literal l ->
      literal l

  | While (c, b) ->
      nest 2 (group (string "while" ++ may_paren_expression c
                     ++ string "{" ^^ break 1
                                      ++ expression b
                                      ++ break 1 ^^ string "}"))

  | Variable x ->
      identifier x

  | Define (x, e1, e2) ->
      nest 2 (
        group (value_definition "val" (x, e1) ^^ string ";"
              ))
      ++ group (expression e2)

  | DefineRec (vs, e) ->
      nest 2 (group (
        string "rec"
        ++ separate_map
          (break 1 ^^ string "and" ^^ break 1) (value_definition "") vs
        ^^ string ";"
      )) ++ group (expression e)

  | Fun (p, e) ->
      nest 2 (group (
        group (string "\\" ^^ function_parameters p ++ string "=>") ++
        group (expression e)
      ))

  | Apply (a, bs) ->
      group (
        parens_at_left_of_application a (expression a)
        ++ parens (separate_map (string "," ^^ break 1) expression bs)
      )

  | IfThenElse (c, t, f) ->
      nest 2 (group (
        group (string "if"
               ++ group (expression c)
               ++ string "then"
              )
        ++ group (expression t)
      ))
      ++ nest 2 (group (
        string "else"
        ++ group (expression f)
        ++ string "fi"
      ))

  | WriteBlock (e1, e2, e3) ->
      expression (Apply (Variable (Id "write_block"), [e1; e2; e3]))

  | ReadBlock (e1, e2) ->
      expression (Apply (Variable (Id "read_block"), [e1; e2]))

  | AllocateBlock e1 ->
      expression (Apply (Variable (Id "allocate_block"), [e1]))

  | Switch (i, bs, default) ->
      group (string "switch" ++ expression i ++ string "in")
      ++ group (
        separate_map (string "|" ^^ break 1) expression (Array.to_list bs)
      ) ++ string "or else" ++ begin match default with
        | None -> string "nothing"
        | Some t -> expression t
      end

and function_parameters xs =
  parens (separate_map (string "," ^^ break 1) identifier xs)

and may_paren_expression e = match e with
  | Fun _ | Define _ | DefineRec _ -> parens (expression e)
  | _ -> expression e

and literal = function
  | LInt x ->
      int x
  | LChar c ->
      char c
  | LString s ->
      string_literal s

and char c =
  group (string "'" ^^ string (Char.escaped c) ^^ string "'")

and string_literal s =
  group (string "\"" ^^ string (String.escaped s) ^^ string "\"")

and parens_at_left_of_application e =
  match e with
  | Apply _ | Variable _ | Literal _ -> fun x -> x
  | _ -> parens

and parens_at_right_of_application e =
  match e with
  | Variable _ | Literal _ -> fun x -> x
  | _ -> parens

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 80 b (f x);
  Buffer.contents b
