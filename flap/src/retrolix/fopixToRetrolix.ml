(** This module implements a compiler from Fopix to Retrolix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Retrolix
module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = T.identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)

(** The compilation environment stores the list of global
    variables (to compute local variables) and a table
    representing a renaming (for alpha-conversion). *)
type environment = IdSet.t * (S.identifier * S.identifier) list

(** Initially, the environment is empty. *)
let initial_environment () = (IdSet.empty, [])

(** [fresh_label ()] returns a new identifier for a label. *)
let fresh_label =
  let c = ref 0 in
  fun () -> incr c; T.Label ("l" ^ string_of_int !c)

(** [fresh_label ()] returns a new identifier for a variable. *)
let fresh_variable =
  let c = ref 0 in
  fun () -> incr c; T.(Id ("X" ^ string_of_int !c))

(**
   Every function in Retrolix starts with a declaration
   of local variables. So we need a way to compute the
   local variables of some generated code. This is the
   purpose of the next function:
*)

(** [locals globals b] takes a set of variables [globals] and returns
    the variables use in the list of instructions [b] which are not
    in [globals]. *)
let locals globals b =
   failwith "Students! This is your job!"

(** [translate' p env] turns a Fopix program [p] into a Retrolix
    program using [env] to retrieve contextual information. *)
let rec translate' p env =
  (** The global variables are extracted in a first pass. *)
  let (globals, renaming) = env in
  let globals = List.fold_left get_globals globals p in
  let env = (globals, renaming) in
  (** Then, we translate Fopix declarations into Retrolix declarations. *)
  let defs = List.map (declaration globals) p in
  (defs, env)

and identifier (S.Id x) = T.Id x

and register r =
  T.((`Register (RId (MipsArch.string_of_register r)) : lvalue))

and get_globals env = function
  | S.DefineValue (x, _) ->
    push env x
  | _ ->
    env

and push env x =
  IdSet.add (identifier x) env

and declaration env = T.(function
  | S.DefineValue (S.Id x, e) ->
    let x = Id x in
    let ec = expression (`Variable x) e in
    let locals = locals env ec in
    DValue (x, (locals, ec))

  | S.DefineFunction (S.FunId f, xs, e) ->
    let x = fresh_variable () in
    let ec = expression (`Variable x) e in
    DFunction (FId f,
               List.map identifier xs,
               (locals env ec,
                ec @ [labelled (Ret (`Variable x))]))
  | S.ExternalFunction (S.FunId f) ->
    DExternalFunction (FId f)
)
(** [expression out e] compiles [e] into a block of Retrolix
    instructions that stores the evaluation of [e] into [out]. *)
and expression out = T.(function
  | S.Literal l ->
    [labelled (Assign (out, Load, [ `Immediate (literal l) ]))]

  | S.Variable (S.Id x) ->
    [labelled (Assign (out, Load, [ `Variable (Id x) ]))]

  | S.Define (S.Id x, e1, e2) ->
    (** Hey student! The following code is wrong in general,
	hopefully, you will implement [preprocess] in such a way that
	it will work, right? *)
    expression (`Variable (Id x)) e1 @ expression out e2

  | S.While (c, e) ->
    let closeLabel = [labelled (Comment (string_of_label (fresh_label ())))] in
    let condReg = `Variable (fresh_variable ()) in
    let condIns = expression condReg c in
    let eIns = ( expression out e ) in 
    let condJump = [ labelled (ConditionalJump (EQ, [ condReg; `Immediate (LInt (Int32.of_int 0)) ],
                                                      label_of_instructions closeLabel, label_of_instructions condIns))] in
    condIns @ condJump @ eIns @ condJump @ closeLabel

  | S.IfThenElse (c, t, f) ->
    let closeLabel = [labelled (Comment (string_of_label (fresh_label ())))] in  
    let insTrue = (expression out t) @ (inst_jump_to_label closeLabel) in
    let insFalse = (expression out f) @ (inst_jump_to_label closeLabel) in
    (condition (label_of_instructions insTrue) (label_of_instructions insFalse) c) @ insTrue @ insFalse @ closeLabel

  | S.FunCall (S.FunId f, es) when is_binop f ->
    assign out (binop f) es

  | S.FunCall (S.FunId f, actuals) ->
       failwith "Students! This is your job!"

  | S.UnknownFunCall (ef, actuals) ->
       failwith "Students! This is your job!"
  | S.Switch (e, cases, default) ->
       failwith "Students! This is your job!"
)

(** This method will extract the label of the first instruction in the labelOfInsList *)
and label_of_instructions labelOfInsList = fst (List.hd labelOfInsList)

and string_of_label (T.Label x) = x

and inst_jump_to_label l =
    [labelled (T.Jump (label_of_instructions l))]

and as_rvalue e =
  let x = `Variable (fresh_variable ()) in
  (x, expression x e)

and as_rvalues rs f =
  let xs, es = List.(split (map as_rvalue rs)) in
  List.flatten es @ f xs

and assign out op rs =
  as_rvalues rs (fun xs ->
    [labelled (T.Assign (out, op, xs))]
  )


and condition lt lf c = T.(
  let x = fresh_variable () in
  expression (`Variable x) c
  @ [ labelled (ConditionalJump (EQ, [ `Variable x;
                                       `Immediate (LInt (Int32.of_int 0)) ],
                                 lf,
                                 lt))]
)

and first_label = function
  | [] -> assert false
  | (l, _) :: _ -> l

and labelled i =
  (fresh_label (), i)

and literal = T.(function
  | S.LInt x ->
    LInt x
  | S.LFun (S.FunId f) ->
    LFun (FId f)
  | S.LChar c ->
    LChar c
  | S.LString s ->
    LString s
)

and is_binop = function
  | "`+" | "`-" | "`*" | "`/" -> true
  | c -> is_condition c

and is_condition = function
  | "`<" | "`>" | "`=" | "`<=" | "`>=" -> true
  | _ -> false

and binop = T.(function
  | "`+" -> Add
  | "`-" -> Sub
  | "`*" -> Mul
  | "`/" -> Div
  | c -> Bool (condition_op c)
)

and condition_op = T.(function
  | "`<" -> LT
  | "`>" -> GT
  | "`<=" -> LTE
  | "`>=" -> GTE
  | "`=" -> EQ
  | _ -> assert false
)

let preprocess p env =
  (p, env)


(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  let p, env = preprocess p env in
  let p, env = translate' p env in
  (p, env)

