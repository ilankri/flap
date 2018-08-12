open PPrint
open PPrintCombinators
open PPrintEngine

open KontixAST

let ( ++ ) x y =
  x ^^ break 1 ^^ y

let located f x = f (Position.value x)

let rec program (p,e) =
  separate_map hardline definition p ++
  group (string "eval" ++ expression e)

and definition = function
  | DefFun (f, xs, e) ->
    nest 2 (
      group (string "def" ++ function_identifier f
             ++ PPrintOCaml.tuple
                  (List.map identifier ("K"::"E"::xs))
             ++ string "=")
      ++ group (expression e)
    )
  | DefCont (f,env,res,e)->
     nest 2
      (group (string "def" ++ function_identifier f
             ++ PPrintOCaml.tuple [environment env;identifier res]
             ++ string "=")
       ++ group (expression e))

and tuple xl = brackets (separate (string ",") xl)

and environment xl =
  tuple (List.map identifier ("K"::"E"::xl))

and identifier x = string x

and function_identifier x = string x

and basicexpr = function
  | Num n -> string (string_of_int n)
  | FunName f -> string ("&" ^ f)
  | Var v -> string v
  | IfThenElse (c, t, f) ->
    nest 2 (
      group (string "if"
             ++ group (basicexpr c)
             ++ string "then"
      )
      ++ group (basicexpr t)
      ++ string "else"
      ++ group (basicexpr f)
    )
  | Let (x, e1, e2) ->
    nest 2 (
      group (
        group (string "let"
               ++ identifier x
               ++ string "="
        )
        ++ group (basicexpr e1)
        ++ string "in"
      )
    )
    ++ group (basicexpr e2)
  | BlockNew e ->
     string "new" ++ brackets (basicexpr e)
  | BlockGet (e1,e2) ->
     group (basicexpr e1) ++ brackets (basicexpr e2)
  | BlockSet (e1,e2,e3) ->
     group (basicexpr e1) ++ brackets (basicexpr e2) ++
     string ":=" ++ group (basicexpr e3)
  | BinOp (op,e1,e2) ->
     group (parens (basicexpr e1 ++ string (binop op) ++ basicexpr e2))
  | Print s -> string ("\""^String.escaped s^"\"")

and expression = function
  | TIfThenElse (c, t, f) ->
    nest 2 (
      group (string "if"
             ++ group (basicexpr c)
             ++ string "then"
      )
      ++ group (expression t)
      ++ string "else"
      ++ group (expression f)
    )
  | TLet (x, e1, e2) ->
    nest 2 (
      group (
        group (string "let"
               ++ identifier x
               ++ string "="
        )
        ++ group (basicexpr e1)
        ++ string "in"
      )
    )
    ++ group (expression e2)
  | TPushCont(f,ids,e) ->
     group
       (string "let K,E = " ++
        string ("&"^f^",") ++
        environment ids ++
        string "in") ++
     group (expression e)
  | TFunCall (e, es) ->
     (match e with FunName f -> string f
                 | _ -> string "?" ++ parens (basicexpr e)) ++
       PPrintOCaml.tuple
         ([string "K"; string "E"] @ List.map basicexpr es)
  | TContCall e ->
     string "K" ++
      PPrintOCaml.tuple [string "E"; basicexpr e]

and binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.5 80 b (f x);
  Buffer.contents b
