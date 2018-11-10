(** Conversion from Fopix to Anfix *)

(** For the moment, we only handle Fopix code which is already
    in ANF form (i.e. all arguments of function call and binop
    are simple). This is used for parsing Anfix without defining
    a full parser (but reusing Fopix parser instead) *)

module S=FopixAST
module T=AnfixAST

type environment = unit
let initial_environment () = ()

type defs = (T.identifier * T.expression) list

let fresh_identifier = Gensym.make "_x"

(* Return true iff the given Fopix expression is already simple.  *)
let is_simple = function
  | S.Literal _ | S.Variable _ -> true
  | _ -> false

(* Return a fresh identifier iff the Fopix expression [e] is simple.  *)
let fresh_identifier_opt e =
  if is_simple e then None else Some (fresh_identifier ())

let rec program l = List.map definition l

and definition = function
  | S.DefineValue (S.Id i,e) -> T.DefVal (i,expr e)
  | S.DefineFunction (S.FunId f,a,e) ->
      T.DefFun (f,List.map (fun (S.Id x) -> x) a,expr e)
  | S.ExternalFunction _ -> ExtStd.failwith_todo __LOC__

and simplexpr : S.expression -> T.simplexpr = function
  | S.Literal (S.LInt n) -> T.Num (Int32.to_int n)
  | S.Literal (S.LFun (S.FunId f)) -> T.FunName f
  | S.Variable (S.Id x) -> T.Var x
  | e -> failwith ("This expression should be simple:" ^
                   FopixPrettyPrinter.(to_string expression e))

and binop = function
  | "`+" -> T.Add
  | "`-" -> T.Sub
  | "`*" -> T.Mul
  | "`/" -> T.Div
  | "`=" -> T.Eq
  | "`<=" -> T.Le
  | "`<" -> T.Lt
  | "`>=" -> T.Ge
  | "`>" -> T.Gt
  | _ -> failwith "Unknown binary operator"

and is_binop fun_id = try fun_id.[0] = '`' with Invalid_argument _ -> false

and expr : S.expression -> T.expression = function
  | S.Literal (S.LInt n) -> T.Simple (T.Num (Int32.to_int n))
  | S.Literal (S.LFun (S.FunId f)) -> T.Simple (T.FunName f)
  | S.Variable (S.Id x) -> T.Simple (T.Var x)
  | S.Define (S.Id x,e1,e2) -> T.Let (x, expr e1, expr e2)
  | S.IfThenElse (e1,e2,e3) ->
      simplify_expr e1 (fun x -> T.IfThenElse(x, expr e2, expr e3))
  | S.FunCall (S.FunId b,[e1; e2]) when is_binop b ->
      simplify_expr e1 (fun x1 ->
        simplify_expr e2 (fun x2 ->
          T.BinOp(binop b, x1, x2)
        )
      )
  | S.FunCall (S.FunId "allocate_block", [e]) ->
      simplify_expr e (fun size ->
        T.BlockNew (size)
      )
  | S.FunCall (S.FunId "read_block", [e1; e2]) ->
      simplify_expr e1 (fun a ->
        simplify_expr e2 (fun i ->
          T.BlockGet (a,i)
        )
      )
  | S.FunCall (S.FunId "write_block", [e1; e2; e3]) ->
      simplify_expr e1 (fun a ->
        simplify_expr e2 (fun i ->
          simplify_expr e3 (fun e ->
            T.BlockSet (a,i,e)
          )
        )
      )
  | S.FunCall (S.FunId "print_string", [S.Literal (S.LString s)]) -> T.Print s
  | S.FunCall (e,el) ->
      simplify_expr (S.Literal (S.LFun e)) (fun f ->
        simplify_exprs el (fun xs ->
          T.FunCall (f, xs)
        )
      )
  | _ -> ExtStd.failwith_todo __LOC__

(* Simplify the Fopix expression [e] (if necessary) and then build
   a more complex expression by applying [f] to the simplification of
   [e].  *)
and simplify_expr (e : S.expression) (f : T.simplexpr -> T.expression) :
  T.expression =
  match fresh_identifier_opt e with
  | None -> f (simplexpr e)
  | Some id -> T.Let (id, expr e, f (T.Var id))

(* Simplify the Fopix expressions [es] (if necessary) and then build
   a more complex expression by applying [f] to the simplifications of
   [es].  *)
and simplify_exprs (es : S.expression list)
    (f : T.simplexpr list -> T.expression) : T.expression =
  let ids = List.map fresh_identifier_opt es in
  let simplexprs = List.map2 (fun id e ->
    match id with
    | None -> simplexpr e
    | Some id -> T.Var id
  ) ids es
  in
  List.fold_right2 (fun id e acc ->
    match id with
    | None -> acc
    | Some id -> T.Let (id, expr e, acc)
  ) ids es (f simplexprs)
