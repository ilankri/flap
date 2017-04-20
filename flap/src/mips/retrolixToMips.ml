(** This module implements a compiler from Retrolix to Mips.

    For details about the procedure call conventions used by GCC for
    MIPS architecture see:

      - doc/resources/MIPS-specification.pdf (section A.6)

      - http://www.cs.umb.edu/cs641/MIPscallconvention.html *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Retrolix
module Target = Mips

type environment = unit

let initial_environment () = ()

module S = Source.AST
module T = Target.AST

let fresh_label =
  let c = ref 0 in
  fun () ->
    incr c;
    T.Label ("__" ^ string_of_int !c)

let labelled instrs = [{T.label = fresh_label (); T.value = instrs}]

let labelled' label instrs = [{T.label = T.Label label; T.value = instrs}]

let int16_literal i = T.Literal (Int16.of_int i)

let sizeof nwords = nwords * MipsArch.word_size

let sizeof' nwords = int16_literal (sizeof nwords)

let load_immediate r i = [T.Li (r, T.Literal (Int16.of_int32 i))]

let increment_sp nwords = [T.Addiu (MipsArch.sp, MipsArch.sp, sizeof' nwords)]

let decrement_sp nwords =
  [T.Addiu (MipsArch.sp, MipsArch.sp, sizeof' (-nwords))]

let sp_offset_address offset =
  T.RegisterOffsetAddress (MipsArch.sp, sizeof' offset)

(** The labels of global variable are prefixed by __global__. *)
let global_variable_label x = "__global__" ^ x

(** [register s] turns a RetrolixHardware register name into a
    register of the target architecture. *)
let register (S.RId s) = MipsArch.register_of_string s

(** [variable_address stacksize env x] returns the address
    of the variable [x]. If [x] is local, this address is
    located inside the stack frame. If [x] is global, this
    address is represented by a label. *)
let variable_address stacksize env ((S.Id s) as x) =
  let offset =
    try Some (List.assoc x env) with
    | Not_found -> None
  in
  match offset with
  | Some offset ->              (* [x] is local.  *)
    sp_offset_address (stacksize - offset)
  | None ->                     (* [x] is global.  *)
    T.LabelAddress (T.Label (global_variable_label s))

(** [load_variable stacksize env r x] emits the instructions
    to load a variable [x] in register [r]. *)
let load_variable stacksize env r x =
  match variable_address stacksize env x with
  | T.LabelAddress l when Options.get_gcc () -> T.([
      Lui (r, LabelAddressHi l);
      Addiu (r, r, LabelAddressLow l);
      Lw (r, RegisterAddress r)
    ])
  | addr -> [
      T.Lw (r, addr)
    ]

(** [tmp1] and [tmp2] have been reserved for this pass. *)
let tmp1 = MipsArch.tmp1
let tmp2 = MipsArch.tmp2

(** [load_rvalue stacksize env rvalue rdest f] inspects [rvalue]
    to determine if it must be loaded into [rdest] before the
    emission of the instruction described by [f]. *)
let load_rvalue stacksize env rvalue rdest f =
  match rvalue with
  | `Register r ->
    f (register r)
  | `Variable x ->
    load_variable stacksize env rdest x
    @ f rdest
  | `Immediate (S.LInt i) ->
    load_immediate rdest i @ f rdest
  | `Immediate (S.LFun (S.FId fl)) when Options.get_gcc () ->
    T.([
        Lui (rdest, LabelAddressHi (Label fl));
        Addiu (rdest, rdest, LabelAddressLow (Label fl))
      ]) @ f rdest
  | `Immediate (S.LFun (S.FId fl)) ->
    T.(La (rdest, LabelAddress (Label fl))) :: f rdest
  | `Immediate _ ->
    failwith "Strings and characters literal are not handled yet."

(** [store_variable stacksize env x r] emits the instructions
    to store the value of a register [r] into a variable [x]. *)
let store_variable (stacksize : int) env x r =
  match variable_address stacksize env x with
  | T.LabelAddress l when Options.get_gcc () -> T.([
      Lui (tmp1, LabelAddressHi l);
      Sw (r, RegisterOffsetAddress (tmp1, LabelAddressLow l))
    ])
  | addr -> [
      T.Sw (r, addr)
    ]

let semantics op =
  let as_int32 = function
    | S.LInt i -> i
    | S.LFun _ | S.LChar _ | S.LString _ -> assert false
  in
  let cond_semantics = function
    | S.GT -> ( > )
    | S.LT -> ( < )
    | S.GTE -> ( >= )
    | S.LTE -> ( <= )
    | S.EQ -> ( = )
  in
  let op =
    match op with
    | S.Add -> Int32.add
    | S.Mul -> Int32.mul
    | S.Div -> Int32.div
    | S.Sub -> Int32.sub
    | S.And -> failwith "TODO"
    | S.Or -> failwith "TODO"
    | S.Bool cond ->
      fun x y ->
        if (cond_semantics cond) (Int32.compare x y) 0 then Int32.one else
          Int32.zero
    | S.Load -> assert false
  in
  fun i j -> op (as_int32 i) (as_int32 j)

let mk_instr binop = fun rdest r1 r2 ->
  match binop with
  | S.Add -> T.Add (rdest, r1, r2)
  | S.Mul -> T.Mul (rdest, r1, r2)
  | S.Div -> T.Div (rdest, r1, r2)
  | S.Sub -> T.Sub (rdest, r1, r2)
  | S.And -> failwith "TODO"
  | S.Or -> failwith "TODO"
  | S.Bool S.GT -> T.Sgt (rdest, r1, r2)
  | S.Bool S.LT -> T.Slt (rdest, r1, r2)
  | S.Bool S.GTE -> T.Sge (rdest, r1, r2)
  | S.Bool S.LTE -> T.Sle (rdest, r1, r2)
  | S.Bool S.EQ -> T.Seq (rdest, r1, r2)
  | S.Load -> assert false

(** [translate p env] turns a Retrolix program into a MIPS program. *)
let rec translate (p : S.t) (env : environment) : T.t * environment =

  (** [block stacksize locals formals instructions] compiles a retrolix
      block into a MIPS block list.  *)
  let rec block stacksize formals locals instructions =
    let env =
      List.mapi (fun i x -> (x, -i)) formals @
      List.mapi (fun i x -> (x, i + 1)) locals
    in
    List.map (fun (S.Label l, i) ->
        {
          T.label = T.Label l;
          T.value = instruction stacksize locals env l i
        }) instructions

  (** [instruction stacksize locals env l i] compiles the retrolix
      instruction [i] whose label is [l] into a list of MIPS
      instructions. [stacksize] is the size of the current stack
      frame and [locals] the list of local variables. *)
  and instruction stacksize locals env l = T.(function
      | S.Call (_, f, rs) ->
        begin match f with
          | `Immediate (S.LFun f) -> call stacksize env f rs
          | _ -> assert false
        end

      | S.TailCall (_, _) -> failwith "TODO"

      | S.Ret _ ->
        (* Pop the stack frame and then jump to the return address.  *)
        free_stack_frame stacksize @ [T.Jr MipsArch.ra]

      | S.Assign (lv, S.Load, [rv]) ->
        (* We store [rv] in [tmp2] because [store_variable] may use
           [tmp1].  *)
        load_rvalue stacksize env rv tmp2 (fun rsrc ->
            match lv with
            | `Register rdest -> [T.Move (register rdest, rsrc)]
            | `Variable x -> store_variable stacksize env x rsrc
          )

      | S.Assign (lv, binop, [rv1; rv2]) ->
        load_rvalue stacksize env lv tmp2 (fun rdest ->
            mk_operation stacksize env rdest rv1 rv2
              (semantics binop) (mk_instr binop rdest) @ (
              match lv with
              | `Register _ -> []
              | `Variable x -> store_variable stacksize env x rdest)
          )

      | S.Assign (_, _, _) -> assert false

      | S.Jump (S.Label l) -> [T.J (T.Label l)]

      | S.ConditionalJump (_, _, _, _) -> failwith "TODO"

      | S.Switch (_, _, _) -> failwith "TODO"

      | S.Comment s -> [T.Comment s]

      | S.Exit -> (load_immediate (MipsArch.v 0) (Int32.of_int 10))@[T.Syscall]
    )

  (** First, push the rvalues [rs] on the stack, then jump to the label [f]
      and finally pop [rs] from the stack.  *)
  and call stacksize env (S.FId f) rs =
    let fst_four_actuals, extra_actuals = MipsArch.split_params rs in
    let push i rv =
      load_rvalue stacksize env rv tmp1 (fun rsrc ->
          [T.Sw (rsrc, sp_offset_address (-i - 1))]
        )
    in
    let nwords = List.length rs in
    (* Note: We have to reverse the list [extra_actuals] to follow the MIPS
       procedure call conventions (see figure A.6.2 of MIPS
       specification for details).  *)
    List.flatten (List.mapi push (List.rev extra_actuals)) @
    decrement_sp nwords @
    T.Jal (T.Label f) ::
    increment_sp nwords

  (** [mk_operation stacksize env rdest r1 r2 semantics make] compiles
      the application of an operation of two rvalues [r1] and [r2] whose
      result is stored in [rdest].

      If the two rvalues are immediate literals, [semantics] is applied
      to directly produce the result.

      Otherwise, [make] is used to emit the assembler instruction corresponding
      to the operation. [load_rvalue] is used to determine if the [rvalues] must
      be first loaded in temporary registers [tmp1] and [tmp2].
  *)
  and mk_operation stacksize env rdest r1 r2 semantics make = S.(
      match r1, r2 with
      | `Immediate i, `Immediate j ->
        load_immediate rdest (semantics i j)

      | r1, r2 ->
        load_rvalue stacksize env r1 tmp1 (fun r1 ->
            load_rvalue stacksize env r2 tmp2 (fun r2 ->
                [make r1 r2]
              )
          )
    )

  (** [function_definition bs df] inserts the compiled code of [df]
      in the block list [bs]. *)
  and function_definition bs = T.(function
      | S.DFunction (S.FId fid, formals, (locals, instrs)) ->
        let allocate_stack_frame, stacksize = allocate_stack_frame locals in
        labelled' fid allocate_stack_frame @
        block stacksize formals locals instrs @
        bs
      | S.DValue _ | S.DExternalFunction _ -> bs
    )

  (** [allocate_stack_frame locals] modifies the stack
      pointer to introduce a fresh stack frame large
      enough to store the variables [locals]. *)
  and allocate_stack_frame locals =
    let nwords = List.length locals in
    (decrement_sp nwords, nwords)

  (** [free_stack_frame size] destructs the latest
      stack frame given the [size] of this stack frame. *)
  and free_stack_frame size = increment_sp size

  (** [extract_global xs d] extracts a global variable definition from [d]
      and inserts it in the list [xs]. *)
  and extract_global xs = function
    | S.DValue (S.Id x, _) -> (T.Label (global_variable_label x)) :: xs
    | _ -> xs
  in

  (**
     [main] is the entry point of the program. It must initialize
     global variables and then call the standard "exit" function with
     0 as an argument.

     To initialize global variables, we simply concatenate the
     compiled code of each variable code block.
  *)
  let main =
    let extract_init_code = function
      | S.DValue (S.Id id, (locals, instrs)) -> (locals, instrs)
      | S.DExternalFunction _ | S.DFunction _ -> ([], [])
    in
    let locals, instrs = List.split (List.map extract_init_code p) in
    let main =
      S.DFunction (S.FId "main", [], (List.flatten locals, List.flatten instrs))
    in
    function_definition [] main @
    labelled (
      load_immediate (MipsArch.a 0) Int32.zero @
      [T.J (T.Label "exit")]
    )
  in

  (** [code] is the program code of [p] compiled in MIPS for GCC. *)
  let code = main @ List.fold_left function_definition [] (List.rev p) in

  let globals = List.fold_left extract_global [] (List.rev p) in
  (T.({ globals; code }), ())
