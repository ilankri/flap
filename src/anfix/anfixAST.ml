(** The abstract syntax tree for anfix programs. *)

(** Anfix is the same as Fopix, except that:
    - function call arguments must be "simple" (variables or consts)
    - same for "if" conditions and primitive arguments
*)

type program = definition list

and definition =
  | DefVal of identifier * expression
  | DefFun of function_identifier * formals * expression

and expression =
  | Simple of simplexpr
  | Let of identifier * expression * expression
  | IfThenElse of simplexpr * expression * expression
  | BinOp of binop * simplexpr * simplexpr
  | BlockNew of simplexpr
  | BlockGet of simplexpr * simplexpr
  | BlockSet of simplexpr * simplexpr * simplexpr
  | FunCall of simplexpr * simplexpr list
  | Print of string

and simplexpr =
  | Num of int
  | FunName of function_identifier
  | Var of identifier

and identifier = string

and formals = identifier list

and function_identifier = string

and binop = (* FopixAST.binop = *)
  | Add | Sub | Mul | Div | Mod (* arithmetic ops *)
  | Eq | Le | Lt | Ge | Gt (* comparisons *)

and t = program
