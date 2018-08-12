(** Conversion from Anfix to a subset of Fopix *)

(** This is used for instance during printing *)

module S=AnfixAST
module T=FopixAST

let rec program l = List.map definition l

and identifier = fun id -> T.Id id

and function_identifier = fun fun_id -> T.FunId fun_id

and definition = function
  | S.DefVal (i,e) -> T.DefineValue (identifier i, expr e)
  | S.DefFun (f,a,e) ->
      T.DefineFunction (function_identifier f, List.map identifier a, expr e)

and binop b =
  function_identifier (
    "`" ^ match b with
    | S.Add -> "+"
    | S.Sub -> "-"
    | S.Mul -> "*"
    | S.Div -> "/"
    | S.Mod -> failwith "TODO"
    | S.Eq -> "="
    | S.Le -> "<="
    | S.Lt -> "<"
    | S.Ge -> ">="
    | S.Gt -> ">"
  )

and expr = function
  | S.Simple e -> simple e
  | S.Let (x,e1,e2) -> T.Define (identifier x,expr e1,expr e2)
  | S.IfThenElse (e1,e2,e3) -> T.IfThenElse (simple e1,expr e2,expr e3)
  | S.BinOp (b,e1,e2) -> T.FunCall (binop b,[simple e1; simple e2])
  | S.BlockNew e -> T.FunCall (function_identifier "allocate_block", [simple e])
  | S.BlockGet (e1,e2) ->
      T.FunCall (function_identifier "read_block", [simple e1; simple e2])
  | S.BlockSet (e1,e2,e3) ->
      T.FunCall (function_identifier "write_block",
                 [simple e1; simple e2; simple e3])
  | S.FunCall (e,el) -> (
      match simple e with
      | T.Literal (T.LFun f) -> T.FunCall (f, List.map simple el)
      | _ -> assert false       (* By typing *)
    )
  | S.Print s ->
      T.FunCall (function_identifier "print_string", [T.Literal (T.LString s)])

and simple = function
  | S.Num n -> T.Literal (T.LInt (Int32.of_int n))
  | S.FunName f -> T.Literal (T.LFun (function_identifier f))
  | S.Var x -> T.Variable (identifier x)
