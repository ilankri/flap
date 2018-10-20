open Position
open Error
open HobixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of hobix evaluates into a [value]. *)
type 'e gvalue =
  | VInt          of Int32.t
  | VChar         of char
  | VString       of string
  | VUnit
  | VAddress      of Memory.location
  | VPrimitive    of string * ('e gvalue list -> 'e gvalue)
  | VBool         of bool
  | VFun          of identifier list * expression * 'e

type ('a, 'e) coercion = 'e gvalue -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_char     = function VChar c -> Some c | _ -> None
let value_as_addr     = function VAddress a -> Some a | _ -> None

let ( >>= ) m f =
  match m with
  | None -> None
  | Some x -> f x

let return x =
  Some x

let trust_me = function
  | None -> assert false (* Impossible. *)
  | Some x -> x

type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value x = VBool x

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
          print_block m d a
      | VFun _ ->
          "<fun>"
      | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
  and print_block m d a =
    let b = Memory.dereference m a in
    let vs = Array.to_list (Memory.array_of_block b) in
    "[ " ^ String.concat "; " (List.map (print_value d) vs) ^ " ]"
  in
  print_value 0 v

module Environment : sig
  type t
  val empty : t
  val bind    : t -> identifier -> t gvalue -> t
  val update  : identifier -> t -> t gvalue -> unit
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> t gvalue
  val last    : t -> (identifier * t gvalue * t) option
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier

  let lookup' x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier x)
      | EBind (y, v, e) ->
          if x = y then v else aux e
    in
    aux

  let lookup x e = !(lookup' x e)

  let update x e v =
    lookup' x e := v

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
    VPrimitive (name, function [VInt x; VInt y] -> out (op x y)
                             | _ -> assert false (* By typing. *)
      )
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
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
    [ ("`=", ( = )); ("`<", ( < )); ("`>", ( > )); ("`>=", ( >= ));
      ("`<=", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, function [VBool x; VBool y] -> out (op x y)
                             | _ -> assert false (* By typing. *)
      )
  in
  let boolarith name = boolbin name (fun x -> VBool x) in
  let boolarithops =
    [ ("`||", ( || )); ("`&&", ( && )) ]
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", function
        | [ VInt x ] -> print (Int32.to_string x)
        | _ -> assert false (* By typing. *)
      )
  in
  let print_string =
    VPrimitive  ("print_string", function
        | [ VString x ] -> print x
        | _ -> assert false (* By typing. *)
      )
  in
  let equal_string =
    VPrimitive  ("equal_string", function
        | [ VString x; VString y ] -> VBool (String.compare x y = 0)
        | _ -> assert false (* By typing. *)
      )
  in
  let equal_char =
    VPrimitive  ("equal_char", function
        | [ VChar x; VChar y ] -> VBool (Char.compare x y = 0)
        | _ -> assert false (* By typing. *)
      )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print_int" print_int
  |> bind' "print_string" print_string
  |> bind' "equal_string" equal_string
  |> bind' "equal_char"   equal_char
  |> bind' "true"         (VBool true)
  |> bind' "false"        (VBool false)
  |> bind' "nothing"      VUnit

let initial_runtime () =
  let bind_bool s b env = Environment.bind env (Id s) (VBool b) in
  {
    memory      = Memory.create (640 * 1024);
    environment =
      primitives
      |> bind_bool "true" true
      |> bind_bool "false" false
  }

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x) ->
    Error.error "interpretation" Position.dummy
      (Printf.sprintf "`%s' is unbound." x)

(* [definition pos runtime d] evaluates the new definition [d]
   into a new runtime [runtime']. In the specification, this
   is the judgment:

                        E, M ⊢ dᵥ ⇒ E', M'

*)
and definition runtime d =
  match d with
  | DefineValue (x, e) ->
      let v = expression runtime.environment runtime.memory e in
      { runtime with environment =
                       bind_identifier runtime.environment x v
      }
  | DefineRecFuns rdefs ->
      { runtime with environment =
                       define_recvalues runtime.environment runtime.memory rdefs
      }

  | DeclareExtern _ ->
      runtime

and define_recvalues environment memory rdefs =
  let environment =
    List.fold_left (fun env (x, _) -> bind_identifier env x VUnit) environment
      rdefs
  in
  let vs = expressions environment memory (snd (List.split rdefs)) in
  List.iter2 (fun (x, _) v ->
      Environment.update x environment v
    ) rdefs vs;
  environment

(* [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression environment memory = function
  | Apply (a, b) ->
      let vbs () = expressions environment memory b in
      begin match expression environment memory a with
      | VPrimitive ("`||", f) ->
          begin match expression environment memory (List.nth b 0) with
          | VBool true -> VBool true
          | _ -> expression environment memory (List.nth b 1)
          end
      | VPrimitive ("`&&", f) ->
          begin match expression environment memory (List.nth b 0) with
          | VBool false -> VBool false
          | _ -> expression environment memory (List.nth b 1)
          end

      | VPrimitive (_, f) ->
          f (vbs ())

      | VFun (xs, e, environment) ->
          expression (List.fold_left2 bind_identifier environment xs (vbs ()))
            memory e

      | _ ->
          assert false (* By typing. *)
      end

  | While (c, e) ->
      let rec aux () =
        match expression environment memory c with
        | VBool true ->
            ignore (expression environment memory e);
            aux ()
        | VBool false ->
            VUnit
        | _ ->
            assert false (* By typing. *)
      in
      aux ()

  | Switch (e, branches, default) ->
      begin match expression environment memory e with
      | VInt i ->
          let i = Int32.to_int i in
          if i < 0 then assert false; (* By typing. *)
          if i < Array.length branches then
            expression environment memory branches.(i)
          else begin match default with
            | None -> assert false; (* By typing. *)
            | Some t -> expression environment memory t
          end
      | _ -> assert false (* By typing. *)
      end

  | Fun (p, e) ->
      VFun (p, e, environment)

  | Literal l ->
      literal l

  | Variable x ->
      Environment.lookup x environment

  | Define (x, ex, e) ->
      let v = expression environment memory ex in
      let environment = bind_identifier environment x v in
      expression environment memory e

  | IfThenElse (c, t, f) ->
      let v = expression environment memory c in
      begin match value_as_bool v with
      | None -> assert false (* By typing. *)
      | Some true -> expression environment memory t
      | Some false -> expression environment memory f
      end

  | DefineRec (rdefs, e) ->
      let environment = define_recvalues environment memory rdefs in
      expression environment memory e

  | AllocateBlock e ->
      begin match expression environment memory e with
      | VInt x ->
          let a = Memory.allocate memory x (VInt Int32.zero) in
          VAddress a
      | _ ->
          assert false (* By typing. *)
      end

  | WriteBlock (b, i, v) ->
      let bv = expression environment memory b in
      (value_as_addr bv >>= fun a ->
       let bi = expression environment memory i in
       value_as_int bi >>= fun i ->
       let bb = expression environment memory v in
       let b = Memory.dereference memory a in
       Memory.write b i bb;
       return (VUnit)
      ) |> trust_me (* By typing. *)

  | ReadBlock (b, i) ->
      let bv = expression environment memory b in
      (value_as_addr bv >>= fun a ->
       let bi = expression environment memory i in
       value_as_int bi >>= fun i ->
       let b = Memory.dereference memory a in
       return (Memory.read b i)
      ) |> trust_me (* By typing. *)

and expressions environment memory es =
  let rec aux vs = function
    | [] ->
        List.rev vs
    | e :: es ->
        let v = expression environment memory e in
        aux (v :: vs) es
  in
  aux [] es

and bind_identifier environment (x : identifier) v =
  Environment.bind environment x v

and literal = function
  | LInt x -> VInt x
  | LChar c -> VChar c
  | LString s -> VString s

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
