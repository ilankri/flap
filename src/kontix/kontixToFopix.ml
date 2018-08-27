(** Conversion from Kontix to a subset of Fopix *)

(** This is used for instance during evaluation *)

module S=KontixAST
module T=FopixAST

let fresh_id =
  let r = ref (-1) in
  fun s -> incr r; s^(string_of_int !r)

let rec program (l,e) =
  List.map definition l @
  [T.DefineFunction
     (T.FunId "_return_",[T.Id "E"; T.Id "x"],T.Variable (T.Id "x"));
   T.DefineValue (T.Id "res",
    T.Define(T.Id "K",T.Literal (T.LFun (T.FunId "_return_")),
             T.Define(T.Id "E",T.FunCall (T.FunId "allocate_block",
                                          [T.Literal (T.LInt 0l)]),
      tailexpr e)))]

and definition = function
  | S.DefFun (f,a,e) ->
      T.DefineFunction (
        T.FunId f,
        List.map (fun x -> T.Id x) ("K"::"E"::a),
        tailexpr e
      )
  | S.DefCont (f,ids,x,e) ->
     let id = fresh_id "__env" in
     T.DefineFunction (
       T.FunId f,
       [T.Id id; T.Id x],
       untuple (T.Variable (T.Id id)) 0 ("K"::"E"::ids) (tailexpr e)
     )

and tuple ids =
  let id = fresh_id "__blk" in
  T.Define (
    T.Id id,
    T.FunCall (T.FunId "allocate_block",
               [T.Literal (T.LInt (Int32.of_int @@ List.length ids))]),
         settuple (T.Variable (T.Id id)) 0 ids (T.Variable (T.Id id)))

and settuple e0 i el e = match el with
  | [] -> e
  | id::el ->
      T.Define (
        T.Id "_",
        T.FunCall (
          T.FunId "write_block",
          [e0; T.Literal (T.LInt (Int32.of_int i)); T.Variable id]),
                     settuple e0 (i+1) el e)

and untuple ptr i ids e = match ids with
  | [] -> e
  | id::ids ->
      T.Define (
        T.Id id,T.FunCall (
          T.FunId "read_block",
          [ptr; T.Literal (T.LInt (Int32.of_int i))]),
          untuple ptr (i+1) ids e)

and tailexpr = function
  | S.TLet (x,e1,e2) -> T.Define (T.Id x, basicexpr e1, tailexpr e2)
  | S.TIfThenElse (e1,e2,e3) ->
      T.IfThenElse (basicexpr e1, tailexpr e2, tailexpr e3)
  | S.TFunCall (e,el) ->
      T.FunCall (
        as_fun_id e,
        [T.Variable (T.Id "K"); T.Variable (T.Id "E")] @ List.map basicexpr el)
  | S.TContCall e ->
      T.UnknownFunCall (
        T.Variable (T.Id "K"), [T.Variable (T.Id "E"); basicexpr e]
      )
  | S.TPushCont (f,ids,e) ->
      T.Define (T.Id "E", tuple (List.map (fun x -> T.Id x)
                                   (["K"; "E"]@ids)),
       T.Define (T.Id "K", T.Literal (T.LFun (T.FunId f)), tailexpr e))

and as_fun_id = function
  | S.FunName f -> T.FunId f
  | _ -> assert false

and binop b =
  T.FunId (
    match b with
  | S.Add -> "`+"
  | S.Sub -> "`-"
  | S.Mul -> "`*"
  | S.Div -> "`/"
  | S.Mod -> failwith "TODO"
  | S.Eq -> "`="
  | S.Le -> "`<="
  | S.Lt -> "`<"
  | S.Ge -> "`>="
  | S.Gt -> "`>"
  )

and basicexpr = function
  | S.Num n -> T.Literal (T.LInt (Int32.of_int n))
  | S.FunName f -> T.Literal (T.LFun (T.FunId f))
  | S.Var x -> T.Variable (T.Id x)
  | S.Let (x,e1,e2) -> T.Define (T.Id x,basicexpr e1, basicexpr e2)
  | S.IfThenElse (e1,e2,e3) ->
     T.IfThenElse (basicexpr e1, basicexpr e2, basicexpr e3)
  | S.BinOp (o,e1,e2) -> T.FunCall (binop o, [basicexpr e1; basicexpr e2])
  | S.BlockNew e -> T.FunCall (T.FunId "allocate_block", [basicexpr e])
  | S.BlockGet (e1,e2) ->
      T.FunCall (T.FunId "read_block", [basicexpr e1; basicexpr e2])
  | S.BlockSet (e1,e2,e3) ->
      T.FunCall
        (T.FunId "write_block", [basicexpr e1; basicexpr e2; basicexpr e3])
  | S.Print s -> T.FunCall (T.FunId "print_string", [T.Literal (T.LString s)])
