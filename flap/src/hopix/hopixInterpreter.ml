open Position
open Error
open HopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of hopix evaluates into a [value]. *)
type 'e gvalue =
  | VBool         of bool
  | VInt          of Int32.t
  | VChar         of char
  | VString       of string
  | VUnit
  | VAddress      of Memory.location
  | VTaggedValues of constructor * 'e gvalue list
  | VPrimitive    of string * ('e gvalue Memory.t -> 'e gvalue list -> 'e gvalue)
  | VFun          of pattern located list * expression located * 'e

type ('a, 'e) coercion = 'e gvalue -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_char     = function VChar c -> Some c | _ -> None

type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x

let primitive name ?(error = fun () -> assert false) coercion wrapper f =
  VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

let print_value m v =
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Int32.to_string x
        | VBool true ->
          "true"
        | VBool false ->
          "false"
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VAddress a ->
          print_array_value d (Memory.dereference m a)
        | VTaggedValues (KId k, []) ->
          k
        | VTaggedValues (KId k, vs) ->
          k ^ "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
        | VFun _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
  and print_array_value d block =
    let r = Memory.read block in
    let n = Memory.size block in
    "[ " ^ String.concat ", " (
      List.(map (fun i -> print_value (d + 1) (r i)) (ExtStd.List.range 0 n)
      )) ^ " ]"
  in
  print_value 0 v

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

type value = Environment.t gvalue

type formals = identifier list

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    VPrimitive (name, fun _ -> function
      | [VInt x; VInt y] -> out (op x y)
      | _ -> assert false (* By typing. *)
    )
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v)) x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Int32.(
    [ ("`+", add); ("`-", sub); ("`*", mul); ("`/", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name (fun x -> VBool x) in
  let cmparithops =
    [ ("`=", ( = )); ("`<", ( < )); ("`>", ( > )); ("`>=", ( >= )); ("`<=", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun m -> function
      | [VBool x; VBool y] -> out (op x y)
      | _ -> assert false (* By typing. *)
    )
  in
  let boolarith name = boolbin name (fun x -> VBool x) in
  let boolarithops =
    [ ("`||", ( || )); ("`&&", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m vs ->
      let repr = String.concat ", " (List.map (print_value m) vs) in
      output_string stdout repr;
      VUnit
    )
  in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> fun env -> Environment.bind env (Id "print") generic_printer
  |> fun env -> Environment.bind env (Id "true")  (VBool true)
  |> fun env -> Environment.bind env (Id "false") (VBool false)

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

(* Exception raised when a pattern matching fails.  *)
exception Pattern_matching_failure of Position.t

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(* [definition pos runtime d] evaluates the new definition [d]
   into a new runtime [runtime']. In the specification, this
   is the judgment:

                        E, M ⊢ dᵥ ⇒ E', M'

*)
and definition runtime d =
  match Position.value d with
  | DefineValue (x, e) ->
    let v, memory = expression' runtime.environment runtime.memory e in
    {
      environment = bind_identifier runtime.environment x v;
      memory = memory
    }
  | DefineType _ | DeclareExtern _ -> runtime
  | DefineRecFuns _ -> failwith "TODO"

and expression' environment memory e =
  expression (position e) environment memory (value e)

(* [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression position environment memory = function
  | Literal l -> (located literal l, memory)

  | Variable id ->
    (Environment.lookup (Position.position id) (value id) environment, memory)

  | Define (id, e1, e2) -> failwith "TODO"

  | DefineRec (defs, e) -> failwith "TODO"

  | Apply (e, _, es) ->
    let fv, memory = expression' environment memory e in
    let vs, memory = expressions environment memory es in
    begin match fv with
      | VPrimitive (_, primitive) -> (primitive memory vs, memory)
      | VFun (ps, e, environment) ->
        let environment = patterns environment memory vs ps in
        expression' environment memory e
      | VBool _ | VInt _ | VChar _ | VString _ | VUnit | VAddress _ |
        VTaggedValues _ ->
        assert false            (* By typing.  *)
    end

  | If (ifs, e) -> failwith "TODO"

  | Fun (FunctionDefinition (_, ps, e)) -> (VFun (ps, e, environment), memory)

  | Tagged (c, _, es) -> failwith "TODO"

  | Case (e, branches) -> failwith "TODO"

  | TypeAnnotation (e, _) -> expression' environment memory e

  | Ref e -> failwith "TODO"

  | Read e -> failwith "TODO"

  | Write (e1, e2) -> failwith "TODO"

  | While (e1, e2) -> failwith "TODO"

and expressions environment memory es =
  let rec aux vs memory = function
    | [] -> (List.rev vs, memory)
    | e :: es ->
      let v, memory = expression' environment memory e in
      aux (v :: vs) memory es
  in
  aux [] memory es

and pattern' environment memory v p =
  pattern (position p) environment memory v (value p)

(* [pattern pos env v p] extends [env] such that the value [v] is
   captured by the pattern [p].  *)
and pattern position environment memory v = function
  | PTypeAnnotation (p, _) -> pattern' environment memory v p

  | PVariable id -> Some (Environment.bind environment (value id) v)

  | PTaggedValue (c, ps) -> failwith "TODO"

  | PWildcard -> Some environment

  | PLiteral l when located literal l = v -> Some environment

  | PLiteral _ -> None

  | POr ps -> failwith "TODO"

  | PAnd ps -> failwith "TODO"

and patterns environment memory vs ps =
  let pattern env v p =
    match env with
    | None -> raise (Pattern_matching_failure (position p))
    | Some env -> pattern' env memory v p
  in
  match List.fold_left2 pattern (Some environment) vs ps with
  | None -> assert false
  | Some env -> env

and bind_identifier environment x v =
  Environment.bind environment (Position.value x) v

and literal = function
  | LInt x -> VInt x
  | LString s -> VString s
  | LChar c -> VChar c
  | LBool b -> VBool b

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
      | None -> assert false (* Absurd. *)
      | Some (x, v, env') ->
        let new_environment = Environment.bind new_environment x v in
        substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

let print_observable runtime observation =
  Environment.print observation.new_memory observation.new_environment
