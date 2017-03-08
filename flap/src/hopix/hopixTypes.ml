open HopixAST

let type_error = Error.error "typechecking"

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

let tvar x =
  ATyVar (TId x)

let fresh =
  let r = ref 0 in
  fun () -> incr r; TId ("'a" ^ string_of_int !r)

let ( --> ) tys ty =
  ATyArrow (tys, ty)

exception NotAFunction

let output_type_of_function = function
  | ATyArrow (_, ty) -> ty
  | _ -> raise NotAFunction

let constant x = TCon x, ATyCon (TCon x, [])
let tcunit,   hunit    = constant "unit"
let tcbool,   hbool    = constant "bool"
let tcint,    hint     = constant "int"
let tcstring, hstring  = constant "string"
let tcchar,   hchar    = constant "char"

let tcref = TCon "cell"
let href ty = ATyCon (tcref, [ty])

exception NotAReference

let type_of_reference_type = function
(**
    | Pattern(p) when conditionP -> return

equals to

    | Pattern(p) -> (match p with | Condition -> return )
*)
  | ATyCon (t, [ty]) when t = tcref ->
    ty
  | _ ->
    raise NotAReference

module TypeVariableSet = Set.Make (struct
  type t = type_variable
  let compare = compare
end)

let rec occurs x = function
  | ATyVar tv -> x = tv
  | ATyCon (_, tys) -> List.exists (occurs x) tys
  | ATyArrow (ins, out) -> List.exists (occurs x) (out :: ins)

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

exception NotAMonotype

let type_of_monotype = function
  | Scheme ([], ty) -> ty
  | _ -> raise NotAMonotype

exception InvalidInstantiation of int * int

let rec substitute phi = function
  | ATyVar tv ->
    (try List.assoc tv phi with Not_found -> ATyVar tv)
  | ATyArrow (ins, out) ->
    ATyArrow (List.map (substitute phi) ins, substitute phi out)
  | ATyCon (t, tys) ->
    ATyCon (t, List.map (substitute phi) tys)

let instantiate_type_scheme (Scheme (ts, ty)) types =
  if List.(length ts <> length types) then
    raise (InvalidInstantiation (List.length ts, List.length types));
  let substitution = List.combine ts types in
  substitute substitution ty

let refresh_type_scheme (Scheme (ts, ty)) =
  let ts' = List.map (fun _ -> fresh ()) ts in
  let phi = List.(map (fun (x, y) -> (x, ATyVar y)) (combine ts ts')) in
  Scheme (ts', substitute phi ty)

exception UnificationFailed of aty * aty

let unify_types ty1 ty2 =
  let rec unify = function
    | [] ->
      []
    | (a, b) :: pbs when a = b ->
      unify pbs
    | (ATyVar x, ty) :: pbs ->
      if occurs x ty then raise (UnificationFailed (ty1, ty2));
      let eliminate_x = substitute [(x, ty)] in
      let phi = unify (List.map (fun (a, b) ->
	(eliminate_x a, eliminate_x b)
      ) pbs)
      in
      (x, ty) :: phi
    | (ty, ATyVar x) :: pbs ->
      unify ((ATyVar x, ty) :: pbs)
    | (ATyArrow (ins1, out1), ATyArrow (ins2, out2)) :: pbs ->
	if List.(length ins1 <> length ins2) then
	  raise (UnificationFailed (ty1, ty2));
	unify (List.combine (out1 :: ins1) (out2 :: ins2) @ pbs)
    | (ATyCon (t1, ts1), ATyCon (t2, ts2)) :: pbs ->
      if t1 <> t2 || List.(length ts1 <> length ts2) then
	raise (UnificationFailed (ty1, ty2));
      unify (List.combine ts1 ts2 @ pbs)
    | _ ->
      raise (UnificationFailed (ty1, ty2))
  in
  let phi = unify [(ty1, ty2)] in
  let phi = List.map (fun (x, ty) -> (x, substitute phi ty)) phi in

  if (substitute phi ty1 <> substitute phi ty2) then (
    Error.global_error "internal" (
      Printf.sprintf "Unification has a bug on:\n %s\nand\n %s, producing phi:\n%s\n"
	(print_aty ty1)
	(print_aty ty2)
	(String.concat ", " (List.map (fun (TId x, ty) -> Printf.sprintf "%s -> %s" x (print_aty ty))
			       phi)
	));
  );
  phi

let guess_instantiation (Scheme (ts1, ty1)) ts2 ty2 =
     failwith "Students! This is your job!"

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

let check_well_formed_type pos env ty =
     failwith "Students! This is your job!"

let internalize_ty env ty =
  let pos = Position.position ty in
  let ty = Position.value ty in
  let aty = aty_of_ty ty in
  check_well_formed_type pos env aty;
  aty

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

let bind_type_variables pos env ts =
  List.fold_left (fun env t ->
      bind_type_variable pos env t
    ) env ts

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
  let env = bind_type_variables Position.dummy env ts in
  let data_constructors = match tdef with
    | Abstract -> []
    | DefineSumType ds -> List.map (fun (k, _) -> Position.value k) ds
  in
  let constructor_definition (k, tys) =
    let atys = List.map (internalize_ty env) tys in
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

exception UnboundConstructor

let lookup_type_scheme_of_constructor x env =
  try
    List.assoc x env.constructors
  with Not_found ->
    raise UnboundConstructor

let initial_typing_environment () =
  empty_typing_environment |>
  List.fold_right (fun ti env -> bind_type_definition ti [] Abstract env) [
    tcbool; tcunit; tcstring; tcchar; tcint
  ] |>
  bind_type_definition (TCon "cell") [TId "'a"] Abstract
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

let print_binding (Id x, Scheme (_, s)) =
  x ^ " : " ^ print_aty s

let print_typing_environment tenv =
  let excluded = initial_typing_environment () in
  let values = List.filter (fun (x, _) ->
    not (List.mem_assoc x excluded.values)
  ) (List.rev tenv.values)
  in
  String.concat "\n" (List.map print_binding values)

