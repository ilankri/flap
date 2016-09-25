open Position
open Error
open HopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of hopix evaluates into a [value]. *)
type value =
  | VInt          of Int32.t
  | VPrimitive    of (value list -> value)

let print_value v =
  match v with
    | VInt x ->
      Int32.to_string x
    | VPrimitive _ ->
      "<primitive>"

module Environment : sig
  type t
  val empty : t
  val bind    : t -> identifier -> value -> t
  val update  : Position.t -> identifier -> t -> value -> unit
  exception UnboundIdentifier of identifier * Position.t
  val lookup  : Position.t -> identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * value ref * t

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

  let print_binding (Id x, v) =
    x ^ " = " ^ print_value !v

  let print e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

type runtime = {
  environment : Environment.t;
  memory : value Memory.t;
}

type observable = {
  new_environment : Environment.t;
  new_memory : value Memory.t;
}

let bind_many env l =
  List.fold_left (fun env (x, v) -> Environment.bind env x v) env l

let binary_arithmetic op vs =
  match vs with
    | [ VInt x; VInt y ] -> VInt (op x y)
    | _ -> assert false (* By typing *)

let primitives = bind_many Environment.empty [
  Id "*", VPrimitive (binary_arithmetic Int32.mul);
  Id "+", VPrimitive (binary_arithmetic Int32.add);
  Id "/", VPrimitive (binary_arithmetic Int32.add);
  Id "-", VPrimitive (binary_arithmetic Int32.sub);
]

let initial_runtime () = {
  memory      = Memory.create (640 * 1024);
  environment = primitives
}

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
      environment = Environment.bind runtime.environment (Position.value x) v;
      memory
    }

and expression' environment memory e =
  expression (position e) environment memory (value e)

(* [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression position environment memory = function
  | Literal l ->
    literal l, memory

  | Variable x ->
    Environment.lookup position x environment, memory

  | Apply (f, args) ->
    let fvalue, memory = expression' environment memory f in
    match fvalue with
      | VPrimitive p ->
	let vargs, memory = expressions' environment memory args in
	p vargs, memory
      | VInt _ ->
	assert false (* By typing. *)

and expressions' environment memory = function
  | [] ->
    [], memory
  | e :: es ->
    let v, memory = expression' environment memory e in
    let vs, memory = expressions' environment memory es in
    v :: vs, memory

and literal = function
  | Literal.LInt x -> VInt x

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
  Environment.print observation.new_environment
