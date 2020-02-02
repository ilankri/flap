(** This module implements a compiler from Kontix to Fopix. *)
let error msg = Util.Error.error "compilation" Util.Position.dummy msg
let cont_init,id_init = "_K00", "identifier_init"

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Kontix
module S = Source.Ast
module Target = Language
module T = Target.Ast
module Dispatcher = Javix.FromFopix.Dispatcher
module Labels = Javix.FromFopix.Labels

type environment = Javix.FromFopix.environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
}


module Env = Javix.FromFopix.Env

module Utils : sig
  val translate_cmpop   : string -> T.cmpop
  val unlabelled_instr  : T.instruction -> T.labelled_instruction
  val unlabelled_instrs : (T.instruction list) -> (T.labelled_instruction list)
  val labelled_instr    : T.label -> T.instruction -> T.labelled_instruction
  val labelled_instrs   :
    T.label -> (T.instruction list) -> (T.labelled_instruction list)
  val new_label         : string -> T.label
  val fun_epilog        : (T.labelled_instruction list)
  val translate_binop_comp_with_new_label : (string -> T.instruction * T.label)
  val translate_binop   : (string -> T.instruction)
  val box_after         : T.instruction list -> T.instruction list
  val bipush_box        : (int -> T.instruction list)
  val unbox_after : T.labelled_instruction list -> T.labelled_instruction list
  val unbox_before      : T.instruction list -> T.instruction list
  val translate_Num     : int -> T.labelled_instruction list
  val translate_FunName :
    S.function_identifier -> T.label -> T.labelled_instruction list
  val translate_Var     : T.var -> T.labelled_instruction list
  val translate_Binop   :
    T.labelled_instruction list -> T.labelled_instruction list -> string ->
    T.labelled_instruction list
  val translate_BlockNew:
    T.labelled_instruction list -> T.labelled_instruction list
  val translate_BlockGet:
    T.labelled_instruction list -> T.labelled_instruction list ->
    T.labelled_instruction list
  val translate_BlockSet:
    T.labelled_instruction list -> T.labelled_instruction list ->
    T.labelled_instruction list -> T.labelled_instruction list
  val translate_Print   : string -> T.labelled_instruction list
  val save_return_address : T.label -> T.labelled_instruction list
  val translate_IfThenElse :
    T.labelled_instruction list * T.labelled_instruction list *
    T.labelled_instruction list ->
    T.labelled_instruction list

end = struct
  let translate_cmpop   = Javix.FromFopix.translate_cmpop
  let unlabelled_instr  = Javix.FromFopix.unlabelled_instr
  let unlabelled_instrs = Javix.FromFopix.unlabelled_instrs
  let labelled_instr    = Javix.FromFopix.labelled_instr
  let labelled_instrs   = Javix.FromFopix.labelled_instrs
  let new_label         = Javix.FromFopix.new_label
  let fun_epilog        = Javix.FromFopix.fun_epilog
  let translate_binop_comp_with_new_label =
    Javix.FromFopix.translate_binop_comp_with_new_label
  let translate_binop      = Javix.FromFopix.translate_binop
  let box_after            = Javix.FromFopix.box_after
  let bipush_box           = Javix.FromFopix.bipush_box
  let unbox_after          = Javix.FromFopix.unbox_after
  let unbox_before         = Javix.FromFopix.unbox_before
  let translate_Num        = Javix.FromFopix.translate_Num
  let translate_FunName    = Javix.FromFopix.translate_FunName
  let translate_Var        = Javix.FromFopix.translate_Var
  let translate_Binop      = Javix.FromFopix.translate_Binop
  let translate_BlockNew   = Javix.FromFopix.translate_BlockNew
  let translate_BlockGet   = Javix.FromFopix.translate_BlockGet
  let translate_BlockSet   = Javix.FromFopix.translate_BlockSet
  let translate_Print      = Javix.FromFopix.translate_Print
  let save_return_address  = Javix.FromFopix.save_return_address
  let translate_IfThenElse = Javix.FromFopix.translate_IfThenElse
end

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
}

let basic_code code varSize stackSize = {
  T.classname = "Fopix";
  T.code = code;
  T.varsize = varSize;
  T.stacksize = stackSize;
}

let string_of_binop = function
  | S.Add -> "`+"
  | S.Sub -> "`-"
  | S.Mul -> "`*"
  | S.Div -> "`/"
  | S.Mod -> Util.ExtStd.failwith_todo __LOC__
  | S.Eq -> "`="
  | S.Le -> "`<="
  | S.Lt -> "`<"
  | S.Ge -> "`>="
  | S.Gt -> "`>"

let rec translate_basicexpr (expr: S.basicexpr) (env: environment) :
  (T.labelled_instruction list) =
  match expr with
  | S.Num i -> Utils.translate_Num i

  | S.FunName fun_id ->
      let fun_label = Env.lookup_function_label fun_id env in
      Utils.translate_FunName fun_id fun_label

  | S.Var id ->
      let v = Env.lookup_variable id env in
      Utils.translate_Var v


  | S.Let (id, expr, expr') ->
      let var, env' = Env.bind_variable env id in
      let instrs =
        translate_basicexpr expr env @ Utils.unlabelled_instrs [T.Astore var] in
      let instrs' = translate_basicexpr expr' env' in instrs @ instrs'

  | S.IfThenElse (cond_expr, then_expr, else_expr) ->
      let cond_codes' = translate_basicexpr cond_expr env in
      let then_codes = translate_basicexpr then_expr env in
      let else_codes = translate_basicexpr else_expr env in
      Utils.translate_IfThenElse (cond_codes', then_codes, else_codes)

  | S.BinOp (binop, left_expr, right_expr) ->
      let insts_left = translate_basicexpr left_expr env in
      let insts_right = translate_basicexpr right_expr env in
      Utils.translate_Binop insts_left insts_right (string_of_binop binop)

  | S.BlockNew size_expr ->
      let size = translate_basicexpr size_expr env in
      Utils.translate_BlockNew size

  | S.BlockGet (array_expr, index_expr) ->
      let a_instrs = translate_basicexpr array_expr env in
      let i_instrs = translate_basicexpr index_expr env in
      Utils.translate_BlockGet a_instrs i_instrs

  | S.BlockSet (array_expr, index_expr, value_expr) ->
      let a_instrs = translate_basicexpr array_expr env in
      let i_instrs = translate_basicexpr index_expr env in
      let v_instrs = translate_basicexpr value_expr env in
      Utils.translate_BlockSet a_instrs i_instrs v_instrs

  | S.Print s -> Utils.translate_Print s

let save_vars env =
  Utils.unlabelled_instrs (
    T.Comment "Save variables onto the stack" ::
    List.map (fun (_,var) -> T.Aload var) env.variables
  )

let restore_vars env =
  Util.ExtStd.List.flat_map (fun (_,var) -> [T.Swap; T.Astore var])
    (List.rev env.variables)

let pass_fun_args args env =
  Utils.unlabelled_instr (T.Comment "Pass function arguments") ::
  Util.ExtStd.List.flat_map (fun arg -> translate_basicexpr arg env) args

let call_fun fun_expr env =
  match fun_expr with
  | S.FunName f ->
      Utils.unlabelled_instrs [T.Goto (Env.lookup_function_label f env)]

  | _ ->
      Utils.unlabelled_instr (T.Comment "Compute the function to call") ::
      translate_basicexpr fun_expr env @
      Utils.unlabelled_instrs [T.Goto Dispatcher.label]

let translate_FunCall fun_expr args env =
  (* this code can be changed when we implement TPushCont and TContCall*)
  pass_fun_args args env @
  call_fun fun_expr env

let rec translate_tailexpr (expr: S.tailexpr) (env: environment) :
  (T.labelled_instruction list) =
  match expr with
  | S.TLet (id, expr, expr') ->
      let var, env' = Env.bind_variable env id in
      let instrs =
        translate_basicexpr expr env @ Utils.unlabelled_instrs [T.Astore var] in
      let instrs' = translate_tailexpr expr' env' in instrs @ instrs'

  | S.TIfThenElse (e_cond, e_then, e_else) ->
      let cond_codes = translate_basicexpr e_cond env in
      let then_codes = translate_tailexpr e_then env in
      let else_codes = translate_tailexpr e_else env in
      Utils.translate_IfThenElse (cond_codes, then_codes, else_codes)

  | S.TPushCont _ -> Util.ExtStd.failwith_todo __LOC__

  | S.TFunCall (fun_expr, args) ->
      translate_FunCall fun_expr args env

  | S.TContCall _ -> Util.ExtStd.failwith_todo __LOC__

let translate_fun_body fun_id body env : (T.labelled_instruction list) =
  Utils.unlabelled_instr (T.Comment ("Body of the function " ^ fun_id)) ::
  (translate_tailexpr body env)

let store_fun_args formals env =
  List.fold_left (fun (instrs, env) formal ->
    let var, env = Env.bind_variable env formal in
    (T.Astore var :: instrs, env)
  ) ([], env) formals

let fun_prolog fun_id formals env =
  let instrs, env = store_fun_args formals (Env.clear_all_variables env) in
  (Utils.labelled_instrs (Env.lookup_function_label fun_id env) (
     T.Comment "Store the arguments in variables" ::
     instrs
   ),
   env)

let translate_definition (def:S.definition) (env: environment) :
  (T.labelled_instruction list) * environment =
  match def with
  | S.DefFun (fun_id, formals, body) ->
      let prolog, env' = fun_prolog fun_id formals env in
      (prolog @ translate_fun_body fun_id body env' @ Utils.fun_epilog, env)

  | S.DefCont (cont_id, formals, id, body) ->
      let prolog, env' = fun_prolog cont_id (id :: formals) env in
      (prolog @ translate_fun_body cont_id body env' @ Utils.fun_epilog, env)

let var_and_stack_size (_p : S.t) (_env : environment) : int * int =
  Util.ExtStd.failwith_todo __LOC__

let collect_function_info prog env =
  let collect_function_info env = function
    | S.DefFun (fun_id, formals, _) ->
        env
        |> Env.bind_function_label fun_id
        |> Env.bind_function_formals fun_id formals
    | S.DefCont (cont_id, formals_env, id, _) ->
        env
        |> Env.bind_function_label cont_id
        |> Env.bind_function_formals cont_id (id :: formals_env)
  in
  List.fold_left collect_function_info env prog

let translate (p : S.t) env : T.t * environment =
  Javix.FromFopix.translate (Kontix.ToFopix.program p) env
(* let defs, main = p in *)
(* let env1 = collect_function_info defs env in *)
(* let env2, defs_instrs = ( *)
(*   List.fold_left (fun (env,instrs) def -> *)
(*     let instr_list,env = translate_definition def env in *)
(*     (env, (instrs @ instr_list)) ) (env1,[]) defs) in *)

(* let main_code = translate_tailexpr main env2 in *)
(* (\* let varSize, stackSize = varAndStack_size p env2 in  *\) *)
(* (\* let pre_code = TODO in *\) *)
(* (\* let post_code = TODO in *\) *)
(* let code = *)
(*   (\* pre_code @ *\) *)
(*   main_code @ *)
(*   (\* post_code @ *\) *)
(*   defs_instrs @ *)
(*   Dispatcher.code () *)
(* in *)
(* (\* (basic_code code varSize stackSize), env' *\) *)
(* (basic_code code 100 1000  ), env2 *)
