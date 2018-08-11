open HopixAST

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

let input_types_of_function = function
  | ATyArrow (tyl, _) -> tyl
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

(** Ensemble de types. Ex :
    g: pour tous 'a, 'b ('a->'b)*'a->'b
      [-------------------------------]
       Type scheme
*)
type aty_scheme = Scheme of type_variable list * aty

let mk_type_scheme ty =
  Scheme (free_type_variables ty, ty)

(** Monomophic type. Ex :
    fun g['a,'b] ( f: 'a->'b, x:'a) : 'b = f(x)
                      [----]
                       Monotype
*)
let monotype ty =
  Scheme ([], ty)

exception NotAMonotype

let type_of_monotype = function
  | Scheme ([], ty) -> ty
  | _ -> raise NotAMonotype

let rec substitute phi = function
  | ATyVar tv ->
    (try List.assoc tv phi with Not_found -> ATyVar tv)
  | ATyArrow (ins, out) ->
    ATyArrow (List.map (substitute phi) ins, substitute phi out)
  | ATyCon (t, tys) ->
    ATyCon (t, List.map (substitute phi) tys)

let type_error = Error.error "typechecking"

type error =
  | UnboundTyCons of Position.t * type_constructor
  | UnboundTyVar of Position.t * type_variable
  | UnboundId of Position.t * identifier
  | UnboundDataCons of Position.t * constructor
  | WrongArityTyCons of Position.t * type_constructor * int * int
  | WrongArityDataCons of Position.t * constructor * int * int
  | WrongArityFunction of Position.t * int * int
  | AlreadyBoundDataCons of Position.t * constructor
  | AlreadyBoundTyVar of Position.t * type_variable
  | InvalidApplication of Position.t
  | MissingTypeAnnotation of Position.t
  | TypeMismatch of Position.t * aty * aty
  | TooPolymorphicType of Position.t
  | POrError of Position.t
  | InvalidTypeInstantiation of Position.t * int * int
  | NonLinearPattern of Position.t * identifier
exception TypeError of error

let raise_type_error err = raise (TypeError err)

let ty_cons_err error pos (TCon s) = error pos "type constructor" s

let ty_var_err error pos (TId s) = error pos "type variable" s

let id_err error pos (Id s) = error pos "identifier" s

let unbound_err pos what which =
  type_error pos (Printf.sprintf "Unbound %s %s." what which)

let data_cons_err error pos (KId s) = error pos "data constructor" s

let wrong_arity_err xarity iarity pos what which =
  type_error pos
    (Printf.sprintf "The %s %s has arity %d and not %d."
       what which xarity iarity)

let already_bound_err pos what which =
  type_error pos (Printf.sprintf "Already bound %s %s." what which)

let report_error = function
  | UnboundTyCons (pos, tc) -> ty_cons_err unbound_err pos tc
  | UnboundTyVar (pos, tv) -> ty_var_err unbound_err pos tv
  | UnboundId (pos, id) -> id_err unbound_err pos id
  | UnboundDataCons (pos, dc) -> data_cons_err unbound_err pos dc
  | WrongArityTyCons (pos, tc, xarity, iarity) ->
    ty_cons_err (wrong_arity_err xarity iarity) pos tc
  | WrongArityDataCons (pos, dc, xarity, iarity) ->
    data_cons_err (wrong_arity_err xarity iarity) pos dc
  | AlreadyBoundTyVar (pos, tv) -> ty_var_err already_bound_err pos tv
  | AlreadyBoundDataCons (pos, dc) -> data_cons_err already_bound_err pos dc
  | InvalidApplication pos ->
    type_error pos "This expression must have a functional type to be applied."
  | MissingTypeAnnotation pos -> type_error pos "A type annotation is missing."
  | TypeMismatch (pos, xty, ity) ->
    type_error pos (
      Printf.sprintf "Type error:\nExpected:\n  %s\nGiven:\n  %s\n"
        (print_aty xty) (print_aty ity)
    )
  | TooPolymorphicType pos ->
    type_error pos
      (Printf.sprintf "The type of this expression is too polymorphic.")
  | POrError pos ->
    type_error pos (
      Printf.sprintf "All patterns of a disjunctive pattern must bind \
                      the same set of identifiers."
    )
  | InvalidTypeInstantiation (pos, xarity, iarity) ->
    type_error pos (
      Printf.sprintf
        "Invalid type instantiation: expected %d type(s) and not %d."
        xarity iarity
    )
  | WrongArityFunction (pos, xarity, iarity) ->
    type_error pos
      (Printf.sprintf "This function has arity %d and not %d." xarity iarity)
  | NonLinearPattern (pos, id) ->
    id_err (fun pos what which ->
        type_error pos (
          Printf.sprintf
            "The %s %s is bound several times in this pattern."  what which
        )
      ) pos id

exception InvalidInstantiation of int * int

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
      (x, substitute phi ty) :: phi
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
      Printf.sprintf "Unification has a bug on:\n %s\nand\n %s, \
                      producing phi:\n%s\n"
        (print_aty ty1)
        (print_aty ty2)
        (String.concat ", "
           (List.map
              (fun (TId x, ty) -> Printf.sprintf "%s -> %s" x (print_aty ty))
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

let is_type_variable_defined pos env tv =
  List.mem tv env.type_variables

let lookup_type_info_of_ty_cons pos tc env =
  try List.assoc tc env.type_constructors with
  | Not_found -> raise_type_error (UnboundTyCons (pos, tc))

let check_ty_cons_arity pos tc xarity iarity =
  if xarity <> iarity then
    raise_type_error (WrongArityTyCons (pos, tc, xarity, iarity))

let check_ty_cons_data_cons pos data_constructors =
  try
    let dc = ExtStd.List.find_duplicate data_constructors in
    raise_type_error (AlreadyBoundDataCons (pos, dc))
  with
  | Not_found -> ()

let rec check_well_formed_type pos env = function
  | ATyVar tv ->
    if not (is_type_variable_defined pos env tv) then
      raise_type_error (UnboundTyVar (pos, tv))
  | ATyCon (tc, ts) ->
    let tc_info = lookup_type_info_of_ty_cons pos tc env in
    check_ty_cons_arity pos tc tc_info.arity (List.length ts);
    check_ty_cons_data_cons pos tc_info.data_constructors
  | ATyArrow (ts, t) ->
    List.iter (check_well_formed_type pos env) ts;
    check_well_formed_type pos env t

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

let print_tenv env =
  Printf.sprintf "tvs: %s\n" (
    String.concat ", " (List.map (fun (TId x) -> x) env.type_variables)
  )

let bind_type_variable pos env tv =
  if List.mem tv env.type_variables then
    raise_type_error (AlreadyBoundTyVar (pos, tv));
  { env with type_variables = tv :: env.type_variables }

let bind_type_variables pos env ts =
  List.fold_left (fun env t ->
      bind_type_variable pos env t
    ) env ts

let bind_value x scheme env = {
  env with values = (x, scheme) :: env.values
}

let lookup_type_scheme_of_value pos x env =
  try
    List.assoc x env.values
  with Not_found ->
    raise_type_error (UnboundId (pos, x))

(** Not sure if useful or not...
    let lookup_type_scheme_of_type_constructors pos k env =
    try
    List.assoc k env.type_constructors
    with Not_found ->
    raise (UnboundIdentifier (pos, k))
*)

let bind_type_definition x ts tdef env =
  let arity = List.length ts in
  let data_constructors = match tdef with
    | Abstract -> []
    | DefineSumType ds -> List.map (fun (k, _) -> Position.value k) ds
  in
  let pre_env =
    let env = bind_type_variables Position.dummy env ts in
    let type_constructors =
      (x, { arity; data_constructors }) :: env.type_constructors
    in
    { env with type_constructors; constructors = [] }
  in
  let constructor_definition (k, tys) =
    let atys = List.map (internalize_ty pre_env) tys in
    let aty =
      let ts = List.map (fun v -> Position.unknown_pos (TyVar v)) ts in
      internalize_ty pre_env (Position.unknown_pos (TyCon (x, ts)))
    in
    let scheme =
      mk_type_scheme (atys --> aty)
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

let lookup_type_scheme_of_constructor pos x env =
  try
    List.assoc x env.constructors
  with Not_found -> raise_type_error (UnboundDataCons (pos, x))

let initial_typing_environment () =
  empty_typing_environment |>
  List.fold_right (fun ti env -> bind_type_definition ti [] Abstract env) [
    tcbool; tcunit; tcstring; tcchar; tcint
  ] |>
  bind_type_definition (TCon "cell") [TId "'a"] Abstract
  |> List.fold_right
    (fun (x, s) env -> bind_value (Id x) (mk_type_scheme s) env) [
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
