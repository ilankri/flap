(** This module implements a compiler from Fopix to Javix. *)

let error msg = Error.error "compilation" Position.dummy msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Javix

module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
type environment = {
  nextvar          : int;
  variables        : (string * T.var) list;
  function_labels  : (string * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (string * string list) list;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
}

module Env : sig
  val lookup_variable : string -> environment -> T.var

  val lookup_last_var : environment -> T.var

  val bind_function_formals :
    string -> string list -> environment -> environment

  (** [lookup_function_label f env] returns the label of [f] in [env]. *)
  val lookup_function_label : string -> environment -> T.label

  (** [lookup_function_formals f env] returns the formal arguments of
      [f] in [env]. *)
  val lookup_function_formals : string -> environment -> string list

  val bind_function_label : string -> environment -> environment

  (** Variables *)

  (** [bind_variable env x] associates Fopix variable x to the next
      available Javix variable, and return this variable and the updated
      environment *)
  val bind_variable : environment -> string -> T.var * environment

  val clear_all_variables : environment -> environment

  val fresh_function_label : string -> T.label

end = struct
  let lookup_variable id env =
    try
      List.assoc id env.variables
    with Not_found -> error ("Variable " ^ id ^ " not found")

  let lookup_last_var env = T.Var (pred env.nextvar)

  let bind_function_formals fun_id formals env = {
    env with function_formals = (fun_id, formals) :: env.function_formals
  }

  let lookup_function_label f env =
    List.assoc f env.function_labels

  let lookup_function_formals f env =
    List.assoc f env.function_formals

  (** [fresh_function_label f] returns a fresh label starting with [f]
      that will be used for the function body instructions. *)
  let fresh_function_label =
    let fresh_suffix = Gensym.make "_body_" in
    fun f -> T.Label (f ^ fresh_suffix ())

  let bind_function_label fun_id env = {
    env with
    function_labels =
      (fun_id, fresh_function_label fun_id) :: env.function_labels
  }

  let bind_variable env x =
    let v = T.Var env.nextvar in
    v,
    { env with
      nextvar = env.nextvar + 1;
      variables = (x,v) :: env.variables }

  let clear_all_variables env = {env with variables = []; nextvar = 0}
end

let unlabelled_instr (instr : T.instruction) : T.labelled_instruction =
  (None, instr)

let unlabelled_instrs instrs = List.map unlabelled_instr instrs

let labelled_instr label instr = (Some label, instr)

(* Return a list of instructions with the given label on the first
   instruction.  *)
let labelled_instrs label = function
  | [] -> []
  | instr :: instrs -> labelled_instr label instr :: unlabelled_instrs instrs

let box_after instrs = instrs @ [T.Box]

let bipush_box i = box_after [T.Bipush i]

let unbox_before instrs = T.Unbox :: instrs

let unbox_after instrs = instrs @ [unlabelled_instr T.Unbox]

let new_label =
  let fresh_suffix = Gensym.make "_" in
  fun name -> T.Label (name ^ fresh_suffix ())

(** For return addresses (or later higher-order functions),
    we encode some labels as numbers. These numbers could then
    be placed in the stack, and will be used in a final tableswitch
    We arbitrarily start these coding numbers at 1000, in order to
    easily distinguish them in javix code, adapt your tableswitch
    accordingly to use 1000 as base value. *)

module Labels :
 sig
   val encode : T.label -> int
   val all_encodings : unit -> (int * T.label) list
 end = struct
   let nextcode = ref 1000
   let allcodes = ref ([]:(int * T.label) list)
   let encode lab =
     let n = !nextcode in
     incr nextcode;
     allcodes := (n,lab) :: !allcodes;
     n
   let all_encodings () = !allcodes
 end

module Dispatcher : sig
  val label : T.label

  val code : unit -> T.labelled_instruction list
end = struct
  let label = T.Label "dispatch"

  let base_value = 1000

  let default_label = T.Label "crash"

  let code () =
    let labels =
      Labels.all_encodings ()
      |> List.filter (fun (_, label) -> label <> default_label)
      |> List.sort (fun (code, _) (code', _) -> compare code code')
      |> List.split
      |> snd
    in
    labelled_instr default_label (T.Comment "Let's crash...") ::
    labelled_instrs label (
      unbox_before [T.Tableswitch (base_value, labels, default_label)]
    )
end

let basic_program code =
  { T.classname = "Fopix";
    T.code = code;
    T.varsize = 100;
    T.stacksize = 10000; }

let translate_binop op =
  let op' = (match op with
      | "`+" -> T.Add
      | "`-" -> T.Sub
      | "`*" -> T.Mul
      | "`/" -> T.Div
      (* | S.Mod -> T.Rem *)
      | _ -> error "Incorrect call: Binop is not an arithmetic operator") in
  T.Binop(op')

let translate_cmpop op = match op with
  | "`=" -> T.Eq
  | "`<=" -> T.Le
  | "`<" -> T.Lt
  | "`>=" -> T.Ge
  | "`>" -> T.Gt
  | _ -> error "Incorrect call: Binop is not a comparison operator"

let translate_binop_comp_with_new_label binop =
  let to_label = new_label "cmpop" in
  (T.If_icmp (translate_cmpop binop, to_label), to_label)

let get_if_true_label_from_cond_codes cbs =
  let _, last = List.nth cbs ((List.length cbs)-1) in
  match last with
  | T.If_icmp (_, label) -> label
  | _ -> error "Last instruction must be If_icmp"

let append_if_icmp cbs =
  match (List.nth cbs ((List.length cbs)-1)) with
  | _, T.If_icmp (_, _) -> cbs
  | _, _ ->
      let code_comp = T.If_icmp (T.Eq, (new_label "cmpop")) in
      unbox_after cbs @ (unlabelled_instrs [T.Bipush 1;code_comp])

let save_vars env =
  unlabelled_instrs (
    T.Comment "Save variables onto the stack" ::
    List.map (fun (_, var) -> T.Aload var) env.variables
  )

let save_return_address return_label =
  unlabelled_instrs (
    T.Comment "Save the return address" ::
    bipush_box (Labels.encode return_label)
  )

let restore_vars env =
  ExtStd.List.flat_map (fun (_, var) -> [T.Swap; T.Astore var])
    (List.rev env.variables)

let translate_Num i = unlabelled_instrs (bipush_box i)

let translate_Var v = unlabelled_instrs [T.Aload v]

let translate_FunName fun_id fun_label =
   unlabelled_instrs (
        T.Comment ("Push the encoded label of the function " ^ fun_id) ::
        bipush_box (Labels.encode fun_label)
      )

let translate_Binop insts_left insts_right binop =
   begin match binop with
      | "`+" | "`-" | "`*" | "`/" ->
          let op = translate_binop binop in
          unbox_after insts_left @ unbox_after insts_right @
          unlabelled_instrs (box_after [op])

      | "`=" | "`<=" | "`<" | "`>=" | "`>" ->
          let op, to_label = translate_binop_comp_with_new_label binop in
          let close_label = new_label "close" in
          unbox_after insts_left @
          unbox_after insts_right @
          unlabelled_instrs [op; T.Bipush 0] @
          unlabelled_instr (T.Goto close_label) ::
          labelled_instr to_label (T.Bipush 1) ::
          labelled_instrs close_label [T.Box]
      | _ -> failwith "Unknown binary operator"
      end

let translate_BlockNew size =
  unlabelled_instr (T.Comment "builds an array of java Objects") ::
      size @
      unlabelled_instrs (unbox_before [T.Anewarray])

let translate_BlockGet a_instrs i_instrs =
  unlabelled_instr (T.Comment "array access: array[index]") ::
      a_instrs @ unlabelled_instr T.Checkarray :: i_instrs @
      unlabelled_instrs (unbox_before [T.AAload])

let translate_BlockSet a_instrs i_instrs v_instrs  =
  unlabelled_instrs (
        T.Comment "array modification: array[index] = value" ::
        bipush_box 0
      ) @
      a_instrs @
      unlabelled_instr T.Checkarray ::
      unbox_after i_instrs @ v_instrs @ unlabelled_instrs [T.AAstore]

let translate_Print s = unlabelled_instrs (box_after [T.Print s])

let translate_IfThenElse (cond_codes, then_codes, else_codes) =
      let cond_codes' = append_if_icmp cond_codes in
      let if_true_label = get_if_true_label_from_cond_codes cond_codes' in
      let close_label = new_label "close" in
      cond_codes' @
      else_codes @
      unlabelled_instr (T.Goto close_label) ::
      labelled_instr if_true_label (T.Comment "then_start") ::
      then_codes @
      labelled_instrs close_label [T.Comment "end_if"]

let is_binop fun_id = try fun_id.[0] = '`' with Invalid_argument _ -> false

(* We translate a Fopix expression into a list of labelled Javix
   instructions.  *)
let rec translate_expression (expr : S.expression) (env : environment) :
  T.labelled_instruction list =
  match expr with
  | S.Literal (S.LInt i) -> translate_Num (Int32.to_int i)

  | S.Variable (S.Id id) ->
      let v  = Env.lookup_variable id env in
      translate_Var v

  | S.Literal (S.LFun (S.FunId f)) ->
      let fun_label = Env.lookup_function_label f env in
      translate_FunName f fun_label

  | S.Define (S.Id id, expr, expr') ->
      let (var, env') = Env.bind_variable env id in
      let instrs =
        translate_expression expr env @ unlabelled_instrs [T.Astore var] in
      let instrs' = translate_expression expr' env' in
      instrs @ instrs'

  | S.IfThenElse (cond_expr, then_expr, else_expr) ->
      let cond_codes' = translate_expression cond_expr env in
      let then_codes = translate_expression then_expr env in
      let else_codes = translate_expression else_expr env in
      translate_IfThenElse (cond_codes', then_codes, else_codes)

  | S.FunCall (S.FunId binop, [left_expr; right_expr]) when is_binop binop ->
      let insts_left = translate_expression left_expr env in
      let insts_right = translate_expression right_expr env in
      translate_Binop insts_left insts_right binop

  | S.FunCall (S.FunId "allocate_block", [size_expr]) ->
      let size = translate_expression size_expr env in
      translate_BlockNew size

  | S.FunCall (S.FunId "read_block", [array_expr; index_expr]) ->
      let a_instrs = translate_expression array_expr env in
      let i_instrs = translate_expression index_expr env in
      translate_BlockGet a_instrs i_instrs

  | S.FunCall (S.FunId "write_block", [array_expr; index_expr; value_expr]) ->
      let a_instrs = translate_expression array_expr env in
      let i_instrs = translate_expression index_expr env in
      let v_instrs = translate_expression value_expr env in
      translate_BlockSet a_instrs i_instrs v_instrs

  | S.FunCall (S.FunId "print_string", [S.Literal (S.LString s)]) ->
      translate_Print s

  | S.FunCall (fun_expr, args) -> fun_call (`Immediate fun_expr) args env

  | S.UnknownFunCall (fun_expr, args) -> fun_call (`Unknown fun_expr) args env

  | _ -> ExtStd.failwith_todo __LOC__

and fun_call fun_expr args env =
  (* TODO: Check if the number of arguments is OK.  In fact, it
     seems impossible to always check this at compile-time without
     a typechecker.  Basically, the problem is that, for
     higher-order function, the called function is determined only
     at runtime.  *)
  let return_label = new_label "return" in
  save_vars env @
  save_return_address return_label @
  pass_fun_args args env @
  call_fun fun_expr env @
  labelled_instrs return_label (
    T.Comment "Returned form the function call" ::
    restore_vars env
  )

and call_fun fun_expr env =
  match fun_expr with
  | `Immediate (S.FunId fun_id) ->
      unlabelled_instrs [T.Goto (Env.lookup_function_label fun_id env)]
  | `Unknown fun_expr ->
      unlabelled_instr (T.Comment "Compute the function to call") ::
      translate_expression fun_expr env @
      unlabelled_instrs [T.Goto Dispatcher.label]

and pass_fun_args args env =
  unlabelled_instr (T.Comment "Pass function arguments") ::
  ExtStd.List.flat_map (fun arg -> translate_expression arg env) args

let collect_function_info prog env =
  let collect_function_info env = function
    | S.DefineFunction (S.FunId fun_id, formals, _) ->
        env
        |> Env.bind_function_label fun_id
        |> Env.bind_function_formals
          fun_id (List.map (fun (S.Id x) -> x) formals)
    | S.DefineValue _ -> env
    | S.ExternalFunction _ -> ExtStd.failwith_todo __LOC__
  in
  List.fold_left collect_function_info env prog

let store_fun_args formals env =
  List.fold_left (fun (instrs, env) formal ->
      let var, env = Env.bind_variable env formal in
      (T.Astore var :: instrs, env)
    ) ([], env) formals

let define_value  id expr env =
  let var, env' = Env.bind_variable env id in
  let instrs =
    translate_expression expr env @ unlabelled_instrs [T.Astore var]
  in
  (instrs, env')

let fun_prolog fun_id formals env =
  let instrs, env = store_fun_args formals (Env.clear_all_variables env) in
  (labelled_instrs (Env.lookup_function_label fun_id env) (
      T.Comment "Store the arguments in variables" ::
      instrs
    ),
   env)

let fun_body fun_id body env =
  unlabelled_instr (T.Comment ("Body of the function " ^ fun_id)) ::
  translate_expression body env

let fun_epilog =
  unlabelled_instrs [
    T.Comment "Return of the function";
    T.Swap;
    T.Goto Dispatcher.label
  ]

(* We translate a Fopix definition into a list of labelled Javix
   instructions and produce a new environment.  *)
let translate_definition (definition : S.definition) (env : environment) :
  (T.labelled_instruction list * environment) =
  match definition with
  | S.DefineValue (S.Id id, expr) ->
      define_value id expr env

  | S.DefineFunction (S.FunId fun_id, formals, body) ->
      let prolog, env' =
        fun_prolog fun_id (List.map (fun (S.Id x) -> x) formals) env
      in
      (prolog @ fun_body fun_id body env' @ fun_epilog, env)

  | S.ExternalFunction _ -> ExtStd.failwith_todo __LOC__

let split_defs p =
  List.fold_right (fun def (vals, defs) ->
      match def with
      | S.DefineValue _ -> (def :: vals, defs)
      | S.DefineFunction _ -> (vals, def :: defs)
      | S.ExternalFunction _ -> ExtStd.failwith_todo __LOC__
    ) p ([], [])

let translate_definitions defs env =
  List.fold_left (fun (code, env) def ->
      let instrs, env = translate_definition def env in
      (code @ instrs, env)
    ) ([], env) defs

(** [translate p env] turns a Fopix program [p] into a Javix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) (env : environment) : T.t * environment =
  let vals, defs = split_defs p in

  (* We need to collect all the function labels in a first pass because
     all the functions are mutually recursive in Fopix.  *)
  let fun_codes, env =
    translate_definitions defs (collect_function_info p env)
  in

  let main_code =
    let main_instrs, env = translate_definitions vals env in
    main_instrs @ unlabelled_instrs (
      T.Comment "Return the value of the last variable" ::
      T.Aload (Env.lookup_last_var env) ::
      unbox_before [T.Ireturn]
    )
  in
  let code = main_code @ fun_codes @ Dispatcher.code () in
  (basic_program code, env)

(** Remarks:
  - When using this compiler from fopix to javix, flap will
    produce some .j files.
    {ol
    {- Compile them to .class via: jasmin Foobar.j}
    {- Run them with: java -noverify Foobar}
    }

  - Final answer:
    your code should contain a final [Ireturn] that should
    return the value of the last DefVal (supposed to be
    an Integer).

  - Function Call Convention:
    {ol
    {- When a function starts, the stack should contain the
      return address (a label encoded as a number, see Labels.encode)
      then the n arguments of the function.}
    {- The function could freely use an modify any variable. So at least
      the variables that are reused after this call should have
      their contents saved in stack before the call and restored
      afterwards.}
    {- The function starts by moving its arguments from the stack to
      some variables.}
    {- When the function returns, the result should be on the top
      of the stack.}
    }

  - Boxing:
    The stack could contain both unboxed elements (Java int)
    or boxed elements (Java objects such as Integer or java arrays).
    We place into variables or in array cells only boxed values.
    The arithmetical operations (iadd, if_icmpeq, ...) only works
    on unboxed numbers.
    Conversion between int and Integer is possible via the
    Box and Unboxed pseudo-instructions (translated into correct
    calls to some ad-hoc methods we provide). You may try to
    do some obvious optimisations such as removing [Box;Unbox] or
    [Unbox;Box].

  - Tail-recursive calls : if the body of f ends with a call to
    another function g (which may be f itself in case of recursion),
    no need to save any variables, nor to push a new return address:
    just reuse the return address of the current call to f when
    jumping to g !

  - Variable size and stack size
    Your code should determine the number of variables used by the
    produced code. You might also try to compute the maximum
    stack size when the code is non-recursive or 100% tail-recursive.

*)
