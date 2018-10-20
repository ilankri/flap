(** The abstract syntax tree for kontix programs. *)

type program = definition list * tailexpr
(** This tailexpr serves as a main.
    At its beginning, the current continuation is _return_ and
    the current environment is empty *)

and definition =
  | DefFun of function_identifier * formals * tailexpr
  | DefCont of function_identifier * formal_env * identifier * tailexpr

and basicexpr =
  | Num of int
  | FunName of function_identifier
  | Var of identifier
  | Let of identifier * basicexpr * basicexpr
  | IfThenElse of basicexpr * basicexpr * basicexpr
  | BinOp of binop * basicexpr * basicexpr
  | BlockNew of basicexpr
  | BlockGet of basicexpr * basicexpr
  | BlockSet of basicexpr * basicexpr * basicexpr
  | Print of string

and tailexpr =
  | TLet of identifier * basicexpr * tailexpr
  | TIfThenElse of basicexpr * tailexpr * tailexpr
  | TPushCont of function_identifier * identifier list * tailexpr
  | TFunCall of basicexpr * basicexpr list
  | TContCall of basicexpr

(** Note:
    - [TPushCont (f,ids,body)] let the current continuation for body
     become f, and the current environment becomes
     [(old K, old E, ids...)].
    - A [FunCall(f,args)] always receives in addition the current
     continuation K and environment E
    - A [ContCall res] launches the current continuation K on the
     current environment E and on the result r. *)


and identifier = string
and function_identifier = string

and formals = identifier list
(** Standard arg names.
    Continuation K and env E aren't mentionned here *)


and formal_env = identifier list
(** Saved variables names.
    (and implicitely a inner continuation K and env E) *)

and binop = AnfixAST.binop =
  | Add | Sub | Mul | Div | Mod (* arithmetic ops *)
  | Eq | Le | Lt | Ge | Gt (* comparisons *)

and t = program
