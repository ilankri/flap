(** This module implements a compiler from Fopix to Retrolix. *)

let error pos msg =
  Util.Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Language
module S = Source.Ast
module T = Target.Ast

(** The compilation environment stores the list of global
    variables (to compute local variables) and a table
    representing a renaming (for alpha-conversion). *)
type environment = T.IdSet.t * (S.identifier * S.identifier) list

let identifier (S.Id x) = T.Id x

let push set x = T.IdSet.add (identifier x) set

let initial_environment () = (T.IdSet.of_list [T.Id "true"; T.Id "false"], [])

(** [fresh_label ()] returns a new identifier for a label. *)
let fresh_label =
  let c = ref 0 in
  fun () -> incr c; T.Label ("l" ^ string_of_int !c)

(** [fresh_variable ()] returns a new identifier for a variable. *)
let fresh_variable =
  let c = ref 0 in
  fun () -> incr c; `Variable (T.(Id ("X" ^ string_of_int !c)))

(** Used by preprocess to generate new id *)
let fresh_id =
  let c = ref 0 in
  fun () ->
    incr c;
    (Printf.sprintf "_retrolix_%d" !c)

let rec preprocess defList env =
  List.fold_left (fun (defs, env) x ->
    let def, env = declaration env x in
    (defs @ [def], env)
  ) ([], env) defList

and check_and_generate_new_id global env x =
  let (globals, renaming) = env in
  let push_global x = if global then push globals x else globals in
  match T.IdSet.find_opt (identifier x) globals with
  | Some _ ->
      let x, renaming = generate_new_id renaming x in
      let env = (push_global x, renaming) in
      (env, x)
  | None -> ((push_global x, renaming), x)

and generate_new_id renaming x =
  let id = S.Id (fresh_id ()) in
  (id, (x, id) :: renaming)

and declaration ((g, r) as env) p = match p with
  | S.DefineValue (x, e) ->
      let env', x = check_and_generate_new_id true env x in
      let _, e = expression env e in
      (S.DefineValue (x, e), env')

  | S.DefineFunction (f, xs, e) ->
      let _, e =
        let globals = List.fold_left push g xs in
        expression (globals, r) e
      in
      (S.DefineFunction (f, xs, e), env)

  | _ -> (p, env)

and fun_expr_list elt (accSet, accList) =
  let env, newE = expression accSet elt in
  (env, newE::accList)

and fun_expr_array (accSet, accArray, c) elt =
  let env, newE = expression accSet elt in
  accArray.(c) <- newE;
  (env, accArray, c+1)

and replace_id_if_need renaming i =
  match List.assoc_opt i renaming with
  | Some id -> id
  | None -> i

and expression env e = match e with
  | S.Variable i ->
      env, S.Variable (replace_id_if_need (snd env) i)

  | S.Define (i, e1, e2) ->
      let env, newE1 = expression env e1 in
      let env, newI = check_and_generate_new_id false env i in
      let env, newE2 = expression env e2 in
      env, S.Define (newI, newE1, newE2)

  | S.FunCall (f, el) ->
      let env, newEl = List.fold_right fun_expr_list el (env, []) in
      env, S.FunCall (f, newEl)

  | S.UnknownFunCall (e, el) ->
      let env, newE = expression env e in
      let env, newEl = List.fold_right fun_expr_list el (env, []) in
      env, S.UnknownFunCall (newE, newEl)

  | S.While (e1, e2) ->
      let env, newE1 = expression env e1 in
      let env, newE2 = expression env e2 in
      env, S.While (newE1, newE2)

  | S.IfThenElse (e, e1, e2) ->
      let env, newE = expression env e in
      let env, newE1 = expression env e1 in
      let env, newE2 = expression env e2 in
      env, S.IfThenElse (newE, newE1, newE2)

  | S.Switch (e, el, eOp) ->
      let env, newE = expression env e in
      let env, newEl, _ = Array.fold_left fun_expr_array (env, el, 0) el in
      let env, newEOp =
        begin
          match eOp with
          | Some x -> let env, e = expression env x in env, Some e
          | None -> env, eOp
        end in
      env, S.Switch (newE, newEl, newEOp)

  | _ -> env, e

(** [translate' p env] turns a Fopix program [p] into a Retrolix
    program using [env] to retrieve contextual information. *)
let rec translate' p ((globals, _) as env) =
  (* Then, we translate Fopix declarations into Retrolix declarations. *)
  let defs = List.map (declaration globals) p in
  (defs, env)

and callee_prologue fst_four_formals =
  let lvs, save_callee_saved =
    save_registers Arch.Mips.callee_saved_registers
  in
  let instrs =
    comment "Save callee-saved registers" ::
    save_callee_saved @
    comment "Retrieve first four actuals" ::
    retrieve_fst_four_actuals fst_four_formals
  in
  (lvs, instrs)

and callee_epilogue lvs =
  comment "Restore callee-saved registers" ::
  restore_registers Arch.Mips.callee_saved_registers lvs

and define_function env f xs e proc_call_conv =
  let fst_four_formals, extra_formals =
    if proc_call_conv then Arch.Mips.split_params xs else ([], xs)
  in
  let extra_formals = List.map identifier extra_formals in
  let instrs = function_body fst_four_formals e proc_call_conv in
  let locals =
    List.filter (fun x -> not (List.mem x extra_formals))
      (T.locals env instrs)
  in
  T.DFunction (T.FId f, extra_formals, (locals, instrs))

and function_body fst_four_formals e proc_call_conv =
  let lvs, callee_prologue =
    if proc_call_conv then callee_prologue fst_four_formals else ([], [])
  in
  let out, callee_epilogue =
    if proc_call_conv then
      (T.register Arch.Mips.return_register, callee_epilogue lvs)
    else (fresh_variable (), [])
  in
  callee_prologue @
  comment "Function body" ::
  expression out e @
  callee_epilogue @
  comment "Return" ::
  (* We return the content of the return register just to make the
     Retrolix interpreter happy...  *)
  [labelled (T.Ret out)]

and declaration env = T.(function
  | S.DefineValue (S.Id x, e) ->
      let x = Id x in
      let ec = expression (`Variable x) e in
      let locals = locals env ec in
      DValue (x, (locals, ec))

  | S.DefineFunction (S.FunId f, xs, e) ->
      define_function env f xs e (Options.get_retromips ())

  | S.ExternalFunction (S.FunId f) ->
      DExternalFunction (FId f)
)

and caller_prologue () =
  let lvs, save_caller_saved =
    save_registers Arch.Mips.caller_saved_registers
  in
  (lvs, comment "Save caller-saved registers" :: save_caller_saved)

and caller_epilogue lvs out =
  comment "Restore caller-saved registers" ::
  restore_registers Arch.Mips.caller_saved_registers lvs @
  comment "Retrieve return value" ::
  [load out (T.register Arch.Mips.return_register)]

and fun_call out f actuals proc_call_conv =
  let fst_four_actuals, extra_actuals =
    if proc_call_conv then Arch.Mips.split_params actuals else ([], actuals)
  in
  let lvs, caller_prologue =
    if proc_call_conv then caller_prologue () else ([], [])
  in
  let caller_epilogue =
    if proc_call_conv then caller_epilogue lvs out else []
  in
  caller_prologue @
  comment "Pass actuals" ::
  pass_fst_four_actuals (fst_four_actuals) @
  as_rvalues extra_actuals (fun extra_actuals ->
    comment "Function call" ::
    [labelled (T.Call (out, f, extra_actuals))]
  ) @
  caller_epilogue

(** [expression out e] compiles [e] into a block of Retrolix
    instructions that stores the evaluation of [e] into [out]. *)
and expression out = T.(function
  | S.Literal l ->
      [labelled (Assign (out, Load, [ `Immediate (literal l) ]))]

  | S.Variable (S.Id "true") ->
      expression out (S.(Literal (LInt (Int32.one))))

  | S.Variable (S.Id "false") ->
      expression out (S.(Literal (LInt (Int32.zero))))

  | S.Variable (S.Id x) ->
      [labelled (Assign (out, Load, [ `Variable (Id x) ]))]

  | S.Define (S.Id x, e1, e2) ->
      (* Hey student! The following code is wrong in general,
         hopefully, you will implement [preprocess] in such a way that
         it will work, right? *)
      expression (`Variable (Id x)) e1 @ expression out e2

  | S.While (c, e) ->
      let closeLabel = [labelled (Comment "Exit While")] in
      let condReg = fresh_variable () in
      let condIns = expression condReg c in
      let eIns = ( expression out e ) in
      let condJump =
        [ labelled (
            ConditionalJump (
              EQ,
              [ condReg; `Immediate (LInt (Int32.of_int 0)) ],
              first_label closeLabel,
              first_label eIns
            )
          )]
      in
      condIns @ condJump @ eIns @ labelled (Jump (first_label condIns)) ::
                                  closeLabel

  | S.IfThenElse (c, t, f) ->
      let closeLabel = [labelled (Comment "Exit If")] in
      let jumpToClose = Jump (first_label closeLabel) in
      let insTrue = (expression out t) @ [labelled jumpToClose] in
      let insFalse = (expression out f) @ [labelled jumpToClose] in
      (condition (first_label insTrue) (first_label insFalse) c) @
      insTrue @ insFalse @ closeLabel

  | S.FunCall (S.FunId "`&&", [e1; e2]) ->
      expression out (S.(IfThenElse (e1, e2, Variable (Id "false"))))

  | S.FunCall (S.FunId "`||", [e1; e2]) ->
      expression out (S.(IfThenElse (e1, Variable (Id "true"), e2)))

  | S.FunCall (S.FunId f, es) when is_binop f ->
      assign out (binop f) es

  | S.FunCall (f, actuals) ->
      let f = `Immediate (literal (S.LFun f)) in
      fun_call out f actuals (Options.get_retromips ())

  | S.UnknownFunCall (ef, actuals) ->
      let code_ptr = fresh_variable () in
      expression code_ptr ef @
      fun_call out code_ptr actuals (Options.get_retromips ())

  | S.Switch (e, cases, default) ->
      let closeLabel = [labelled (Comment "Exit Switch")] in
      let jumpToClose = [labelled (Jump (first_label closeLabel))] in
      let condVar, condIns = as_rvalue e in
      let lsOfCases =
        Array.map (fun c -> ((expression out c) @ jumpToClose)) cases
      in
      let labelsOfLsOfCases = Array.map (fun l -> first_label l) lsOfCases in
      let casesIns = Array.fold_left (fun acc i -> i @ acc ) [] lsOfCases in
      match default with
      | Some expr -> (
          let defaultIns = (expression out expr) @ jumpToClose in
          condIns @
          [labelled (
             Switch(condVar, labelsOfLsOfCases, Some(first_label defaultIns))
           )] @
          casesIns @ defaultIns @ closeLabel)
      | None ->
          condIns @ [labelled (Switch(condVar, labelsOfLsOfCases, None))] @
          casesIns @ closeLabel
)

and retrieve_fst_four_actuals fst_four_formals =
  let instrs, _, _ =
    Util.ExtStd.List.asymmetric_map2
      (fun formal ai -> load (`Variable (identifier formal)) (T.register ai))
      fst_four_formals
      Arch.Mips.argument_passing_registers
  in
  instrs

and comment s = labelled (T.Comment s)

and load lv rv = labelled (T.Assign (lv, T.Load, [rv]))

and save_registers regs =
  let save_register reg lv = load lv (T.register reg) in
  let lvs = List.map (fun _ -> fresh_variable ()) regs in
  (lvs, List.map2 save_register regs lvs)

and restore_registers regs lvs =
  let restore_register reg lv = load (T.register reg) lv in
  List.map2 restore_register regs lvs

and inst_jump_to_label l =
  [labelled (T.Jump (first_label l))]

and pass_fst_four_actuals fst_four_actuals =
  let instrs, _, _ =
    Util.ExtStd.List.asymmetric_map2
      (fun ai actual -> expression (T.register ai) actual)
      Arch.Mips.argument_passing_registers fst_four_actuals
  in
  List.flatten instrs

and as_rvalue e =
  let x = fresh_variable () in
  (x, expression x e)

and as_rvalues rvs f =
  let xs, es = List.(split (map as_rvalue rvs)) in
  List.flatten es @ f xs

and assign out op rvs =
  as_rvalues rvs (fun xs ->
    [labelled (T.Assign (out, op, xs))]
  )

and condition lt lf c = T.(
  let x = fresh_variable () in
  expression x c
  @ [ labelled (ConditionalJump (EQ, [ x;
                                       `Immediate (LInt (Int32.of_int 0)) ],
                                 lf,
                                 lt))]
)

and first_label = function
  | [] -> assert false
  | (l, _) :: _ -> l

and labelled i =
  (fresh_label (), i)

and rename_predefine_function f =
  match f with
  (* To handle different function name change from fopix to retrolix :
      allocate_block -> block_create
      write_block -> block_set
      read_block -> block_get *)
  | "allocate_block" -> "block_create"
  | "write_block" -> "block_set"
  | "read_block" -> "block_get"
  | _ -> f

and literal = T.(function
  | S.LInt x ->
      LInt x
  | S.LFun (S.FunId f) ->
      let f =
        if Options.get_retromips () then rename_predefine_function f else f
      in
      LFun (FId f)
  | S.LChar c ->
      LChar c
  | S.LString s ->
      LString s
)

and is_binop = function
  | "`+" | "`-" | "`*" | "`/" | "`||" | "`&&" -> true
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

(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  let p, env = preprocess p env in
  let p, env = translate' p env in
  (* let p = RetrolixRegisterAllocation.translate p in *)
  (p, env)
