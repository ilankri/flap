(** This module implements a compiler from Retrolix to Mips.

    For details about the procedure call conventions used by GCC for
    MIPS architecture see:

      - doc/resources/MIPS-specification.pdf (section A.6)

      - http://www.cs.umb.edu/cs641/MIPscallconvention.html

    In the code below, we always denote by [stacksize] the size in words
    of the current stack frame.  Offsets in the stack frame are taken
    from the top of the frame.  Thus, if a variable [x] is stored at
    offset [i] in the stack frame, then the address of [x] is [sp + 4 *
    i] where [sp] is the current stack pointer.  *)

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

let labelled label instrs = {T.label = label; T.value = instrs}

let int16_literal i = T.Literal (Int16.of_int i)

let arg_reg_count = List.length MipsArch.argument_passing_registers

let sizeof word_count = int16_literal (word_count * MipsArch.word_size)

let load_immediate r i =
  try [T.Li (r, T.Literal (Int16.of_int32 i))] with
  | Int16.LiteralExceeds16bits _ ->
      [ T.Lui (r, T.Literal (Int16.hi i))
      ; T.Xori (r, r, T.Literal (Int16.low i)) ]

let increment_sp word_count =
  [T.Addiu (MipsArch.sp, MipsArch.sp, sizeof word_count)]

let decrement_sp word_count =
  [T.Addiu (MipsArch.sp, MipsArch.sp, sizeof (-word_count))]

let sp_offset_address offset =
  T.RegisterOffsetAddress (MipsArch.sp, sizeof offset)

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
      sp_offset_address offset
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
  | `Immediate _ -> ExtStd.failwith_todo __LOC__

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
    | S.And -> ExtStd.failwith_todo __LOC__
    | S.Or -> ExtStd.failwith_todo __LOC__
    | S.Bool cond ->
        fun x y ->
          if (cond_semantics cond) (Int32.compare x y) 0 then Int32.one else
            Int32.zero
    | S.Load -> assert false
  in
  fun i j -> op (as_int32 i) (as_int32 j)

let mk_instr_by_op binop = fun rdest r1 r2 ->
  match binop with
  | S.Add -> T.Add (rdest, r1, r2)
  | S.Mul -> T.Mul (rdest, r1, r2)
  | S.Div -> T.Div (rdest, r1, r2)
  | S.Sub -> T.Sub (rdest, r1, r2)
  | S.And -> ExtStd.failwith_todo __LOC__
  | S.Or -> ExtStd.failwith_todo __LOC__
  | S.Bool S.GT -> T.Sgt (rdest, r1, r2)
  | S.Bool S.LT -> T.Slt (rdest, r1, r2)
  | S.Bool S.GTE -> T.Sge (rdest, r1, r2)
  | S.Bool S.LTE -> T.Sle (rdest, r1, r2)
  | S.Bool S.EQ -> T.Seq (rdest, r1, r2)
  | S.Load -> assert false

(** [allocate_stack_frame size] modifies the stack pointer to
    introduce a fresh stack frame large enough to store [size]
    variables.  *)
let allocate_stack_frame size = decrement_sp size

(** [free_stack_frame size] destructs the latest
    stack frame given the [size] of this stack frame. *)
let free_stack_frame size = increment_sp size

(** [extract_global xs d] extracts a global variable definition from [d]
    and inserts it in the list [xs]. *)
let extract_global xs = function
  | S.DValue (S.Id x, _) -> (T.Label (global_variable_label x)) :: xs
  | _ -> xs

(** First, push the rvalues [rs] on the stack and then jump to the code
    of [f].  *)
let call stacksize env f rs =
  let push i rv =
    load_rvalue stacksize env rv tmp1 (fun rsrc ->
        [T.Sw (rsrc, sp_offset_address (i + arg_reg_count))]
      )
  in
  let jump = function
    | `Immediate (S.LFun _ as f) ->
        load_rvalue stacksize env (`Immediate f) tmp1 (fun r ->
            [T.Jalr r]
          )
    | `Variable _ | `Register _ ->
        load_rvalue stacksize env f tmp1 (fun r -> [T.Jalr r])
    | `Immediate (S.LInt _ | S.LChar _ | S.LString _) -> assert false
  in
  List.flatten (List.mapi push rs) @ jump f

(** [mk_operation stacksize env rdest r1 r2 semantics make] compiles
    the application of an operation of two rvalues [r1] and [r2] whose
    result is stored in [rdest].

    If the two rvalues are immediate literals, [semantics] is applied
    to directly produce the result.

    Otherwise, [make] is used to emit the assembler instruction corresponding
    to the operation. [load_rvalue] is used to determine if the [rvalues] must
    be first loaded in temporary registers [tmp1] and [tmp2].
*)
let mk_operation stacksize env rdest r1 r2 semantics make = S.(
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

(** [translate p env] turns a Retrolix program into a MIPS program. *)
let rec translate (p : S.t) (env : environment) : T.t * environment =

  (** [block stacksize formals locals instructions] compiles a retrolix
      block into a MIPS block list.  *)
  let rec block stacksize formals locals instructions =
    let env =
      List.mapi (fun i x -> (x, i + stacksize + arg_reg_count)) formals @
      List.mapi (fun i x -> (x, stacksize - i - 1)) locals
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
      | S.Call (_, f, rs) -> call stacksize env f rs

      | S.TailCall (_, _) -> ExtStd.failwith_todo __LOC__

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
                (semantics binop) (mk_instr_by_op binop rdest) @ (
                match lv with
                | `Register _ -> []
                | `Variable x -> store_variable stacksize env x rdest)
            )

      | S.Assign (_, _, _) -> assert false

      | S.Jump (S.Label l) -> [T.J (T.Label l)]

      (* | S.ConditionalJump (c, [rv1; rv2], l1, l2) -> *)
      (*   let label (S.Label l) = T.Label l in *)
      (*   load_rvalue stacksize env rv1 tmp1 (fun r -> *)
      (*       load_rvalue stacksize env rv2 tmp2 (fun s -> [ *)
      (*             mk_instr (S.Bool c) r r s; *)
      (*             T.Beqz (r, label l2); *)
      (*             T.J (label l1) *)
      (*           ] *)
      (*         ) *)
      (*     ) *)

      | S.ConditionalJump (c, rvl, l1, l2) ->
          let extract_op c r1 r2 l1 l2 = match c with
            | S.GT -> [T.Bgt (r1,r2,l1); T.J l2]
            | S.LT -> [T.Bgt (r1,r2,l2); T.J l1]
            | S.GTE -> [T.Bge (r1,r2,l1); T.J l2]
            | S.LTE -> [T.Bge (r1,r2,l2); T.J l1]
            | S.EQ -> [T.Seq (r1,r1,r2); T.Beqz (r1,l2); T.J l1]
          in
          begin match rvl with
          | [r1; r2] ->
              load_rvalue stacksize env r1 tmp1 (fun r1 ->
                  load_rvalue stacksize env r2 tmp2 (fun r2 ->
                      let label (S.Label l) = T.Label l in
                      extract_op c tmp1 tmp2 (label l1) (label l2)
                    )
                )
          | _ -> assert false
          end

      | S.Switch (_, _, _) -> ExtStd.failwith_todo __LOC__

      | S.Comment s -> [T.Comment s]

      | S.Exit ->
          load_immediate (MipsArch.a 0) Int32.zero @ [T.J (T.Label "exit")]
    )

  (** [function_definition bs df] inserts the compiled code of [df]
      in the block list [bs]. *)
  and function_definition bs = T.(function
      | S.DFunction (S.FId fid, formals, (locals, instrs)) ->
          let max_argc instrs =
            let aux acc (_, instr) = S.(
                match instr with
                | Call (_, _, rvs) | TailCall (_, rvs) ->
                    max acc (List.length rvs)
                | Ret _ | Assign _ | Jump _ | ConditionalJump _ | Switch _ |
                  Comment _ | Exit -> acc
              )
            in
            arg_reg_count + List.fold_left aux 0 instrs
          in
          let stacksize = List.length locals + max_argc instrs in
          labelled (T.Label fid) (allocate_stack_frame stacksize) ::
          block stacksize formals locals instrs @
          bs
      | S.DValue _ | S.DExternalFunction _ -> bs
    )
  in

  (**
     [main] is the entry point of the program. It must initialize
     global variables and then call the standard "exit" function with
     0 as an argument.

     To initialize global variables, we simply concatenate the
     compiled code of each variable code block.
  *)
  let main =
    let extract_locals acc = function
      | S.DValue (_, (locals, _)) -> locals @ acc
      | S.DExternalFunction _ | S.DFunction _ -> acc
    in
    let extract_instrs acc = function
      | S.DValue (_, (_, instrs)) -> instrs @ acc
      | S.DExternalFunction _ | S.DFunction _ -> acc
    in
    let locals = List.fold_left extract_locals [] p in
    let instrs =
      List.fold_left extract_instrs [FopixToRetrolix.labelled S.Exit]
        (List.rev p)
    in
    function_definition [] (S.DFunction (S.FId "main", [], (locals, instrs)))
  in

  (** [code] is the program code of [p] compiled in MIPS for GCC. *)
  let code = main @ List.fold_left function_definition [] (List.rev p) in

  let globals = List.fold_left extract_global [] (List.rev p) in
  (T.({ globals; code }), ())
