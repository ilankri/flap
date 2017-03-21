(* This module implements a compiler from Retrolix to Mips *)

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

(** The labels of global variable are prefixed by __global__. *)
let global_variable_label x =
  "__global__" ^ x

(** [translate p env] turns a Retrolix program into a MIPS program. *)
let rec translate (p : S.t) (env : environment) : T.t * environment =

  (** [block stacksize locals instructions] compiles a retrolix block
      into a MIPS block. *)
  let rec block stacksize locals instructions =
    let env = List.mapi (fun i x -> (x, i)) locals in
    List.map (fun (S.Label l, i) ->
              {
                T.label = T.Label l;
                T.value = instruction stacksize locals env l i
             }) instructions

  (** [instruction stacksize locals env l i] compiles the retrolix
      instruction [i] whose label is [l] into a list of MIPS
      instructions. [stacksize] is the size of the current stack
      frame and [locals] the list of local variables. *)
  and instruction stacksize locals env l =
  failwith "Student! This is your job!"

  and call stacksize env f rs =
  failwith "Student! This is your job!"

  and load_immediate r i =
       failwith "Student! This is your job!"


  (** [tmp1] and [tmp2] have been reserved for this pass. *)
  and tmp1 = MipsArch.tmp1
  and tmp2 = MipsArch.tmp2

  (** [register s] turns a RetrolixHardware register name into a
      register of the target architecture. *)
  and register (S.RId s) =
    MipsArch.register_of_string s

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

  (** [load_rvalue stacksize env rvalue rdest f] inspects [rvalue]
      to determine if it must be loaded into [rdest] before the
      emission of the instruction described by [f]. *)
  and load_rvalue stacksize env rvalue rdest f =
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

  (** [variable_address stacksize env x] returns the address
      of the variable [x]. If [x] is local, this address is
      located inside the stack frame. If [x] is global, this
      address is represented by a label. *)
  and variable_address stacksize env ((S.Id s) as x) : address =
       failwith "Student! This is your job!"

  (** [load_variable stacksize env r x] emits the instructions
      to load a variable [x] in register [r]. *)
  and load_variable stacksize env r x =
    match variable_address stacksize env x with
      | T.LabelAddress l when Options.get_gcc () -> T.([
        Lui (r, LabelAddressHi l);
        Addiu (r, r, LabelAddressLow l);
        Lw (r, RegisterAddress r)
      ])
      | addr -> [
        T.Lw (r, addr)
      ]

  (** [store_variable stacksize env x r] emits the instructions
      to store the value of a register [r] into a variable [x]. *)
  and store_variable (stacksize : int) env x r =
    match variable_address stacksize env x with
      | T.LabelAddress l when Options.get_gcc () -> T.([
        Lui (tmp1, LabelAddressHi l);
        Sw (r, RegisterOffsetAddress (tmp1, LabelAddressLow l))
      ])
      | addr -> [
        T.Sw (r, addr)
      ]

  (** [function_definition bs df] inserts the compiled code of [df]
      in the block list [bs]. *)
  and function_definition bs =
    failwith "Student! This is your job!"

  (** [allocate_stack_frame locals] modifies the stack
      pointer to introduce a fresh stack frame large
      enough to store the variables [locals]. *)
  and allocate_stack_frame locals =
       failwith "Student! This is your job!"

  (** [free_stack_frame size] destructs the latest
      stack frame given the [size] of this stack frame. *)
  and free_stack_frame size =
       failwith "Student! This is your job!"

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
       failwith "Student! This is your job!"
  in
  (** [code] is the program code of [p] compiled in MIPS for GCC. *)
  let code =
    main
    @ List.fold_left function_definition [] (List.rev p)
  in
  let globals = List.fold_left extract_global [] p in
  (T.({ globals; code }), ())
