open HopixAST

(** Abstract syntax for types.

    The following internal syntax for types is isomorphic
    to the type [ty] defined in {!HopixAST} except that
    all positions have been erased.

*)
type aty =
  | ATyVar   of type_variable
  | ATyCon   of type_constructor * aty list
  | ATyArrow of aty list * aty

let rec aty_of_ty = function
  | TyVar x            -> ATyVar x
  | TyCon (t, ts)      -> ATyCon (t, List.map aty_of_ty' ts)
  | TyArrow (ins, out) -> ATyArrow (List.map aty_of_ty' ins, aty_of_ty' out)
and aty_of_ty' x = aty_of_ty (Position.value x)

let tvar x =
  ATyVar (TId x)

let ( --> ) tys ty =
  ATyArrow (tys, ty)

let constant x = TCon x, ATyCon (TCon x, [])
let tcunit,   hunit    = constant "unit"
let tcbool,   hbool    = constant "bool"
let tcint,    hint     = constant "int"
let tcstring, hstring  = constant "string"
let tcchar,   hchar    = constant "char"

module TypeVariableSet = Set.Make (struct
  type t = type_variable
  let compare = compare
end)

let free_type_variables ty =
  let rec aux accu = function
    | ATyVar tv -> TypeVariableSet.add tv accu
    | ATyCon (_, tys) -> aux' accu tys
    | ATyArrow (ins, out) -> aux' (aux accu out) ins
  and aux' accu = function
    | [] -> accu
    | ty :: tys -> aux' (aux accu ty) tys
  in
  TypeVariableSet.elements (aux TypeVariableSet.empty ty)

type aty_scheme = Scheme of type_variable list * aty

let mk_type_scheme ty =
  Scheme (free_type_variables ty, ty)

let monotype ty =
  Scheme ([], ty)

type typing_environment = {
  values            : (identifier * aty_scheme) list;
  constructors      : (constructor * aty_scheme) list;
  type_constructors : (type_constructor * type_information) list;
  type_variables    : type_variable list;
}
and type_information = {
  arity             : int;
  data_constructors : constructor list
}

exception UnboundTypeConstructor of Position.position * type_constructor

let check_well_formed_type env ty =
  (* FIXME: Wrong! Student, fix this! *)
  ()

let internalize_ty env ty =
  check_well_formed_type env ty;
  aty_of_ty ty

let empty_typing_environment = {
  values = [];
  constructors = [];
  type_constructors = [];
  type_variables = []
}

exception AlreadyBoundTypeVariable of Position.position * type_variable

let bind_type_variable pos env tv =
  if List.mem tv env.type_variables then
    raise (AlreadyBoundTypeVariable (pos, tv));
  { env with type_variables = tv :: env.type_variables }

let is_type_variable_defined pos env tv =
  List.mem tv env.type_variables

let bind_value x scheme env = {
  env with values = (x, scheme) :: env.values
}

exception UnboundIdentifier of Position.position * identifier

let lookup_type_scheme_of_value pos x env =
  try
    List.assoc x env.values
  with Not_found ->
    raise (UnboundIdentifier (pos, x))

let bind_type_definition x ts tdef env =
  let arity = List.length ts in
  let data_constructors = match tdef with
    | Abstract -> []
    | DefineSumType ds -> List.map (fun (k, _) -> Position.value k) ds
  in
  let constructor_definition (k, tys) =
    let atys = List.map (fun ty -> internalize_ty env (Position.value ty)) tys in
    let scheme =
      mk_type_scheme (atys --> ATyCon (x, List.map (fun v -> ATyVar v) ts))
    in
    (Position.value k, scheme)
  in
  let constructors = match tdef with
    | Abstract -> []
    | DefineSumType ds -> List.map constructor_definition ds @ env.constructors
  in
  let type_constructors =
    (x, { arity; data_constructors }) :: env.type_constructors
  in
  { env with type_constructors; constructors }

exception UnboundConstructor of Position.position * constructor

let lookup_type_scheme_of_constructor pos x env =
  try
    List.assoc x env.constructors
  with Not_found ->
    raise (UnboundConstructor (pos, x))

let initial_typing_environment () =
  empty_typing_environment |>
  List.fold_right (fun ti env -> bind_type_definition ti [] Abstract env) [
    tcbool; tcunit; tcstring; tcchar; tcint
  ] |>
  bind_type_definition (TCon "ref") [TId "'a"] Abstract
  |> List.fold_right (fun (x, s) env -> bind_value (Id x) (mk_type_scheme s) env) [
    "true"     , hbool;
    "false"    , hbool;
    "nothing"  , hunit;
    "print_int", [hint] --> hunit;
    "print_string", [hstring] --> hunit;
    "print", [tvar "'a"] --> hunit;
    "`||", [hbool; hbool] --> hbool;
    "`&&", [hbool; hbool] --> hbool;
    "`=",  [hint; hint] --> hbool;
    "`<=", [hint; hint] --> hbool;
    "`>=", [hint; hint] --> hbool;
    "`<",  [hint; hint] --> hbool;
    "`>",  [hint; hint] --> hbool;
    "`+",  [hint; hint] --> hint;
    "`*",  [hint; hint] --> hint;
    "`-",  [hint; hint] --> hint;
    "`/",  [hint; hint] --> hint;
  ]

let rec print_aty = function
  | ATyVar (TId x) ->
    x
  | ATyArrow (ins, out) ->
    String.concat " * " (List.map print_aty' ins)
    ^ " -> " ^ print_aty' out
  | ATyCon (TCon x, []) ->
    x
  | ATyCon (TCon x, ts) ->
    x ^ "(" ^ String.concat ", " (List.map print_aty' ts) ^ ")"
and print_aty' = function
  | (ATyArrow (_, _)) as t -> "(" ^ print_aty t ^ ")"
  | x -> print_aty x

let print_binding (Id x, Scheme (_, s)) =
  x ^ " : " ^ print_aty s

let print_typing_environment tenv =
  String.concat "\n" (List.map print_binding tenv.values)

