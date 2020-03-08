open Util.Error
open Ast

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

let error' msg = error [] msg

(** Every expression of fopi evaluates into a [value]. *)
type value =
  | VUnit
  | VInt       of Int32.t
  | VBool      of bool
  | VChar      of char
  | VString    of string
  | VAddress   of Common.Memory.location
  | VFun       of function_identifier
  | VPrimitive of string * (value list -> value)

type 'a coercion = value -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_address  = function VAddress x -> Some x | _ -> None
let value_as_unit     = function VUnit -> Some () | _ -> None

type 'a wrapper = 'a -> value
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let address_as_value x = VAddress x
let unit_as_value () = VUnit

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
          print_block m d a
      | VFun _ ->
          "<fun>"
      | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
  and print_block m d a =
    let b = Common.Memory.dereference m a in
    let vs = Array.to_list (Common.Memory.array_of_block b) in
    "[ " ^ String.concat "; " (List.map (print_value d) vs) ^ " ]"
  in
  print_value 0 v

module Environment : sig
  type t
  val initial : t
  val bind    : t -> identifier -> value -> t
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : value Common.Memory.t -> t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let _ =
    Printexc.register_printer (function
      | UnboundIdentifier (Id x) ->
          Some (Printf.sprintf "Unbound identifier %s" x)
      | _ -> None
    )

  let lookup x e =
    try
      List.assoc x e
    with Not_found ->
      raise (UnboundIdentifier x)

  let last = function
    | [] -> None
    | (x, v) :: e -> Some (x, v, e)

  let print_binding memory (Id x, v) =
    (* Identifiers starting with '_' are reserved for the compiler.
       Their values must not be observable by users. *)
    if x.[0] = '_' then
      ""
    else
      x ^ " = " ^ print_value memory v

  let print memory env =
    String.concat "\n" (
      List.(filter (fun s -> s <> "") (map (print_binding memory) env))
    )

end

type runtime = {
  memory : value Common.Memory.t;
  environment : Environment.t;
  functions   : (function_identifier * (formals * expression)) list;
}

type observable = {
  new_environment : Environment.t;
}

module Machine = Util.StateMonad.Make (struct type t = runtime end)

let initial_runtime () =
  let bind_bool s b env = Environment.bind env (Id s) (VBool b) in
  let bind_unit s env = Environment.bind env (Id s) VUnit in
  {
    memory = Common.Memory.create (640 * 1024);
    environment =
      Environment.initial
      |> bind_bool "true"  true
      |> bind_bool "false" false
      |> bind_unit "nothing";
    functions  = [];
  }

let rec evaluate ast =
  let open Machine.Infix in
  Machine.get >>= fun runtime ->
  bind_functions ast >>= fun () ->
  declarations ast >>= fun () ->
  Machine.get >>= fun runtime' ->
  Machine.return @@ extract_observable runtime runtime'

and bind_function = function
  | DefineValue _ -> Machine.return ()

  | DefineFunction (f, xs, e) ->
      Machine.modify (fun runtime ->
        { runtime with
          functions = (f, (xs, e)) :: runtime.functions
        })

  | ExternalFunction _ ->
      Machine.return () (* FIXME: bind to internal primitives later. *)

and bind_functions fs =
  let open Machine.Infix in
  List.fold_left (fun acc f ->
    acc >>= fun () -> bind_function f
  ) (Machine.return ()) fs

and declaration = Machine.Infix.(function
  | DefineValue (i, e) ->
      expression e >>= fun v ->
      Machine.modify (fun runtime ->
        { runtime with environment = Environment.bind runtime.environment i v })
  | DefineFunction _ -> Machine.return ()
  | ExternalFunction _ -> Machine.return ()
)

and declarations ds =
  let open Machine.Infix in
  List.fold_left (fun acc d ->
    acc >>= fun () -> declaration d
  ) (Machine.return ()) ds

and arith_operator_of_symbol = function
  | "`+" -> Int32.add
  | "`-" -> Int32.sub
  | "`/" -> Int32.div
  | "`*" -> Int32.mul
  | _ -> assert false

and cmp_operator_of_symbol = function
  | "`<" -> ( < )
  | "`>" -> ( > )
  | "`<=" -> ( <= )
  | "`>=" -> ( >= )
  | "`=" -> ( = )
  | _ -> assert false

and boolean_operator_of_symbol = function
  | "`&&" -> ( && )
  | "`||" -> ( || )
  | _ -> assert false

and evaluation_of_binary_symbol = function
  | ("`+" | "`-" | "`*" | "`/") as s ->
      arith_binop (arith_operator_of_symbol s)
  | ("`<" | "`>" | "`<=" | "`>=" | "`=") as s ->
      arith_cmpop (cmp_operator_of_symbol s)
  | ("`||" | "`&&") as s ->
      fun e1 e2 ->
        let open Machine.Infix in
        expression e1 >>= fun v1 ->
        begin match value_as_bool v1 with
        | Some false ->
            if s = "`||" then expression e2 else Machine.return v1
        | Some true ->
            if s = "`&&" then expression e2 else Machine.return v1
        | _ -> assert false
        end
  | _ -> assert false

and is_binary_primitive = function
  | "`+" | "`-" | "`*" | "`/" | "`<" | "`>" | "`<=" | "`>=" | "`="
  | "`&&" | "`||" -> true
  | _ -> false

and expression = Machine.Infix.(function
  | Literal l ->
      literal l

  | Variable (Id "true") -> Machine.return @@ VBool true

  | Variable (Id "false") -> Machine.return @@ VBool false

  | Variable x ->
      Machine.get >>= fun runtime ->
      begin try Machine.return @@ Environment.lookup x runtime.environment with
      | Environment.UnboundIdentifier (Id id) ->
          error' ("Unbound identifier " ^ id ^ ".")
      end

  | While (cond, e) ->
      let rec loop () =
        expression cond >>= function
        | VBool true -> expression e >>= fun _ -> loop ()
        | VBool false -> Machine.return ()
        | _ ->
            assert false (* By typing. *)
      in
      loop () >>= fun () -> Machine.return VUnit

  | Switch (e, bs, default) ->
      expression e >>= fun v ->
      begin match value_as_int v with
      | None -> error' "Switch on integers only."
      | Some i ->
          let i = Int32.to_int i in
          if i < Array.length bs then
            expression bs.(i)
          else match default with
            | Some t -> expression t
            | None -> error' "No default case in switch."
      end

  | IfThenElse (c, t, f) ->
      expression c >>= fun v ->
      begin match value_as_bool v with
      | None -> error' "'If' should have a condition that return boolean"
      | Some b -> if b then expression t else expression f
      end

  | Define (x, ex, e) ->
      expression ex >>= fun v ->
      Machine.modify (fun runtime ->
        { runtime with
          environment = Environment.bind runtime.environment x v
        }
      ) >>= fun () ->
      expression e

  | FunCall (FunId "allocate_block", [size]) ->
      expression size >>= fun l ->
      begin
        match l with
        | VInt i ->
            Machine.get >>= fun runtime ->
            let addr = Common.Memory.allocate runtime.memory i VUnit in
            Machine.return @@ VAddress addr
        | _ -> error' "'allocate_block' should have a size in type Literal(int)"
      end

  | FunCall (FunId "read_block", [location; index]) ->
      expression location >>= fun v ->
      begin match value_as_address v with
      | Some addr ->
          expression index >>= fun v ->
          begin match value_as_int v with
          | Some i ->
              Machine.get >>= fun runtime ->
              Machine.return @@
              Common.Memory.read
                (Common.Memory.dereference runtime.memory addr) i
          | None -> error' "A block index must be an integer."
          end
      | None -> error' "A block must be an address."
      end

  | FunCall (FunId "equal_string", [e1; e2]) ->
      expression e1 >>= fun v1 ->
      expression e2 >>= fun v2 ->
      begin match v1, v2 with
      | VString s1, VString s2 ->
          Machine.return @@ VBool (String.compare s1 s2 = 0)
      | _ -> assert false (* By typing. *)
      end

  | FunCall (FunId "equal_char", [e1; e2]) ->
      expression e1 >>= fun v1 ->
      expression e2 >>= fun v2 ->
      begin match v1, v2 with
      | VChar s1, VChar s2 -> Machine.return @@ VBool (Char.compare s1 s2 = 0)
      | _ -> assert false (* By typing. *)
      end

  | FunCall (FunId "print_int", [e]) ->
      expression e >>= fun v ->
      begin match  v with
      | VInt x -> print_string (Int32.to_string x)
      | _ -> assert false (* By typing. *)
      end

  | FunCall (FunId "print_string", [e]) ->
      expression e >>= fun v ->
      begin match v with
      | VString s -> print_string s
      | _ -> assert false (* By typing. *)
      end

  | FunCall (FunId "write_block", [location; index; e]) ->
      expression location >>= fun v1 ->
      expression index >>= fun v2 ->
      begin
        match v1, v2 with
        | VAddress addr, VInt i ->
            expression e >>= fun e ->
            Machine.get >>= fun runtime ->
            Common.Memory.(write (dereference runtime.memory addr) i e);
            Machine.return VUnit
        | _ ->
            error' "'write_block' should have 3 parameters as \
                    (VAddress, VInt, expression)"
      end

  | FunCall (FunId (("`&&" | "`||") as binop), [e1; e2]) ->
      expression e1 >>= fun v1 ->
      begin match v1, binop with
      | VBool true, "`&&" | VBool false, "`||" -> expression e2
      | VBool false, "`&&" -> Machine.return @@ VBool false
      | VBool true, "`||" -> Machine.return @@ VBool true
      | _, _ -> assert false (* By typing. *)
      end

  | FunCall (FunId s, [e1; e2]) when is_binary_primitive s ->
      evaluation_of_binary_symbol s e1 e2

  | FunCall (FunId id as f, es) ->
      Machine.get >>= fun runtime ->
      (try Machine.return @@ List.assoc f runtime.functions with
       | Not_found ->
           error' ("Undefined function " ^ id ^ ".")) >>= fun (formals, body) ->
      let bind_arg acc formal e =
        acc >>= fun () ->
        expression e >>= fun v ->
        Machine.modify (fun runtime -> {
            runtime with
            environment = Environment.bind runtime.environment formal v
          })
      in
      (try List.fold_left2 bind_arg (Machine.return ()) formals es with
       | Invalid_argument _ ->
           error'
             ("Wrong number of arguments given to " ^ id ^ "." )) >>= fun () ->
      expression body >>= fun v ->
      Machine.put runtime >>= fun () ->
      Machine.return v

  | UnknownFunCall (e, es) -> (
      expression e >>= fun v ->
      match v with
      | VFun f -> expression (FunCall (f, es))
      | _ -> assert false
    ))

and binop
  : type a b.
    a coercion -> b wrapper -> (a -> a -> b) -> _ -> _ -> value Machine.t
  = fun coerce wrap op l r ->
    let open Machine.Infix in
    expression l >>= fun lv ->
    expression r >>= fun rv ->
    match coerce lv, coerce rv with
    | Some li, Some ri -> Machine.return @@ wrap (op li ri)
    | _, _ ->
        error' "Invalid binary operation."

and arith_binop l r = binop value_as_int int_as_value l r
and arith_cmpop l r = binop value_as_int bool_as_value l r
and boolean_binop l r = binop value_as_bool bool_as_value l r

and literal l =
  Machine.return @@
  match l with
  | LInt x -> VInt x
  | LString s -> VString s
  | LChar c -> VChar c
  | LFun f -> VFun f

and print_string s =
  output_string stdout s;
  flush stdout;
  Machine.return VUnit

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
      substract Environment.initial runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print runtime.memory observation.new_environment
