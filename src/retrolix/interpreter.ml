(** This module implements the interpreter of the Retrolix programming
    language. *)

open Util.Error
open Ast

let error msg =
  global_error "retrolix execution" msg

(** ----------------------- *)
(** {1 Runtime definition } *)
(** ----------------------- *)

(** This exception is raised to stop the machine. *)
exception ExitNow

(** *)
type data =
  | DUnit
  | DInt   of Int32.t
  | DBool of bool
  | DString of string
  | DChar of char
  | DLocation of Common.Memory.location
  | DFun of function_identifier

let print_data m data =
  let max_depth = 5 in
  let rec print_value d v =
    if d >= max_depth then "..."
    else match v with
      | DUnit -> "()"
      | DInt x -> Int32.to_string x
      | DLocation l -> print_block (d + 1) l
      | DFun (FId f) -> "@" ^ f
      | DBool true -> "true"
      | DBool false -> "false"
      | DChar c -> "'" ^ Char.escaped c ^ "'"
      | DString s -> "\"" ^ String.escaped s ^ "\""
  and print_block d a =
    let b = Common.Memory.dereference m a in
    let vs = Array.to_list (Common.Memory.array_of_block b) in
    "[ " ^ String.concat "; " (List.map (print_value d) vs) ^ " ]"
  in
  print_value 0 data

let type_of = function
  | DUnit -> "unit"
  | DInt _ -> "int"
  | DLocation _ -> "location"
  | DFun _ -> "function_ptr"
  | DChar _ -> "char"
  | DString _ -> "string"
  | DBool _ -> "bool"

let coercion_error expectation v =
  error ("Expecting " ^ expectation ^ " get " ^ type_of v)

let as_unit = function DUnit -> () | v -> coercion_error "unit" v
let as_int  = function DInt x -> x   | v -> coercion_error "int" v
let as_loc  = function DLocation x -> x | v -> coercion_error "location" v
let as_fun  = function DFun f -> f | v -> coercion_error "function_ptr" v

let from_unit ()    = DUnit
let from_int x      = DInt x
let from_location x = DLocation x
let from_fid x      = DFun x

let is_intermediate (Id x) = (x.[0] = 'X')

module IdMap = Map.Make (struct
    type t = identifier
    let compare = compare
  end)

module RIdMap = Map.Make (struct
    type t = register
    let compare = compare
  end)

module FIdMap = Map.Make (struct
    type t = function_identifier
    let compare = compare
  end)

type runtime = {
  return         : data option;
  gvariables     : data IdMap.t;
  lvariables     : data IdMap.t;
  registers      : data RIdMap.t;
  mutable memory : data Common.Memory.t;
  functions      : function_definition FIdMap.t
}

and function_definition = {
  formals : identifier list;
  body : block;
}

type observable = {
  new_variables : data IdMap.t
}

module Machine = Util.StateMonad.Make (struct type t = runtime end)

let initial_runtime () = {
  return     = None;
  gvariables = IdMap.empty;
  lvariables = IdMap.empty;
  registers  = RIdMap.empty;
  memory     = Common.Memory.create (640 * 1024);
  functions  = FIdMap.empty;
}

let print_runtime runtime =
  let idmap m =
    String.concat "," (List.map (fun (Id s, v) ->
      Printf.sprintf "%s = %s" s (print_data runtime.memory v)
    ) (IdMap.bindings m))
  in
  let ridmap m =
    String.concat "," (List.map (fun (RId s, v) ->
      Printf.sprintf "%s = %s" s (print_data runtime.memory v)
    ) (RIdMap.bindings m))
  in
  let return = function
    | None -> "none"
    | Some v -> print_data runtime.memory v
  and gvariables = idmap
  and lvariables = idmap
  and registers = ridmap
  in
  Printf.sprintf "\
  return = %s\n\
  gvariables = %s\n\
  lvariables = %s\n\
  registers = %s\n\
"
    (return runtime.return)
    (gvariables runtime.gvariables)
    (lvariables runtime.lvariables)
    (registers runtime.registers)

(** -------------------------- *)
(** {1 Instruction execution } *)
(** -------------------------- *)

let evaluate (ast : t) =
  let open Machine.Infix in
  let extract_function_definition = function
    | DValue _ -> Machine.return ()
    | DFunction (f, formals, body) ->
        Machine.modify (fun runtime ->
          { runtime with functions =
                           FIdMap.add f { formals; body } runtime.functions
          })
    | DExternalFunction _ -> Machine.return ()
  in
  let extract_function_definitions ds =
    List.fold_left (fun acc d ->
      acc >>= fun () -> extract_function_definition d
    ) (Machine.return ()) ds
  in
  let rec program ds =
    extract_function_definitions ds >>= fun () ->
    definitions ds
  and definition = function
    | DValue (x, b) ->
        block b >>= fun () ->
        Machine.modify (fun runtime ->
          begin match runtime.return with
          | None ->
              runtime
          | Some v ->
              { runtime with gvariables = IdMap.add x v runtime.gvariables }
          end)
    | DFunction _ | DExternalFunction _ -> Machine.return ()
  and definitions ds =
    List.fold_left (fun acc d ->
      acc >>= fun () -> definition d
    ) (Machine.return ()) ds
  and block b =
    let jump_table = Hashtbl.create 13 in
    let rec make = function
      | [(l, i)] ->
          Hashtbl.add jump_table l (i, None)
      | (l, i) :: ((l', _) :: _ as is) ->
          Hashtbl.add jump_table l (i, Some l');
          make is
      | [] -> assert false
    in
    make (snd b);
    Machine.get >>= fun runtime ->
    let locals0 = runtime.lvariables in
    let locals = fst b in
    let start = Hashtbl.find jump_table (fst (List.hd (snd b))) in
    bind_locals
      locals
      (List.map (fun _ -> DInt Int32.zero) locals) >>= fun () ->
    instruction jump_table start >>= fun () ->
    Machine.modify (fun runtime -> { runtime with lvariables = locals0 })

  and instruction jump_table (i, next) =
    let jump l =
      try
        instruction jump_table (Hashtbl.find jump_table l)
      with Not_found ->
        let Label l = l in
        failwith (Printf.sprintf "Label %s not found" l)
    in
    let continue () =
      match next with
      | None -> Machine.return ()
      | Some l -> jump l
    in
    match i with
    | Call (x, f, rs) ->
        rvalue f >>= fun f ->
        rvalues rs >>= fun rs ->
        call f rs >>= fun y ->
        assign x y >>=
        continue
    | TailCall (f, rs) ->
        rvalue f >>= fun f ->
        rvalues rs >>= fun rs ->
        call f rs >>= fun _ ->
        continue ()
    | Ret r ->
        rvalue r >>= fun r ->
        Machine.modify (fun runtime -> { runtime with return = Some r })
    | Assign (x, o, rs) ->
        rvalues rs >>= fun rs ->
        op o rs >>= fun r ->
        assign x r >>=
        continue
    | Jump l -> jump l
    | ConditionalJump (c, rs, l1, l2) ->
        rvalues rs >>= fun rs ->
        condition c rs >>= fun c ->
        if c then jump l1 else jump l2
    | Comment _ -> continue ()
    | Switch (r, ls, default) ->
        rvalue r >>= fun r ->
        begin match r with
        | DInt x ->
            let x = Int32.to_int x in
            if  x < Array.length ls then
              jump ls.(x)
            else
              begin match default with
              | None -> failwith "Non exhaustive switch."
              | Some l -> jump l
              end
        | _ ->
            assert false (* By typing. *)
        end
    | Exit -> Machine.return ()
  and rvalue = function
    | `Variable x ->
        Machine.get >>= fun runtime ->
        (try Machine.return @@ IdMap.find x runtime.lvariables
         with Not_found ->
           (try
              Machine.return @@ IdMap.find x runtime.gvariables
            with Not_found ->
              let Id x = x in
              failwith (Printf.sprintf "Variable %s not found" x)
           )
        )
    | `Register x ->
        (try
           Machine.get >>= fun runtime ->
           Machine.return @@ RIdMap.find x runtime.registers
         with Not_found -> Machine.return @@ DInt Int32.zero
        )
    | `Immediate l -> Machine.return @@ literal l
  and rvalues rs =
    List.fold_right (fun r acc ->
      rvalue r >>= fun r -> acc >|= fun acc -> r :: acc
    ) rs (Machine.return [])
  and op o vs =
    match o, vs with
    | Load, [ v ] -> Machine.return v
    | Add, [ DInt x; DInt y ] -> Machine.return @@ DInt (Int32.add x y)
    | Mul, [ DInt x; DInt y ] -> Machine.return @@ DInt (Int32.mul x y)
    | Div, [ DInt x; DInt y ] -> Machine.return @@ DInt (Int32.div x y)
    | Sub, [ DInt x; DInt y ] -> Machine.return @@ DInt (Int32.sub x y)
    | Bool c, vs ->
        condition c vs >>= fun c ->
        Machine.return @@ DInt (if c then Int32.one else Int32.zero)
    | _, _ ->
        assert false

  and condition op vs =
    Machine.return @@
    match op, vs with
    | GT, [ DInt x1; DInt x2 ] -> x1 > x2
    | LT, [ DInt x1; DInt x2 ] -> x1 < x2
    | GTE, [ DInt x1; DInt x2 ] -> x1 >= x2
    | LTE, [ DInt x1; DInt x2 ] -> x1 <= x2
    | EQ, [ DInt x1; DInt x2 ] -> x1 = x2
    | _,  _ -> assert false

  and literal = function
    | LInt x -> DInt x
    | LFun f -> DFun f
    | LString s -> DString s
    | LChar c -> DChar c

  and assign lvalue v =
    Machine.modify (fun runtime ->
      match lvalue with
      | `Variable x ->
          if IdMap.mem x runtime.lvariables then
            { runtime with lvariables = IdMap.add x v runtime.lvariables }
          else
            { runtime with gvariables = IdMap.add x v runtime.gvariables }
      | `Register x ->
          { runtime with registers = RIdMap.add x v runtime.registers })

  and call fv vs =
    match fv with
    | DFun f ->
        Machine.get >>= fun runtime -> (
          try
            let fdef = FIdMap.find f runtime.functions in
            bind_locals fdef.formals vs >>= fun () ->
            block fdef.body >>= fun () ->
            Machine.get >>= fun runtime ->
            Machine.return
              (match runtime.return with None -> assert false | Some x -> x)
          with Not_found ->
            (if Options.get_retromips () then
               let rs =
                 List.map
                   (fun r -> `Register (RId (Arch.Mips.string_of_register r)))
                   Arch.Mips.argument_passing_registers
               in
               rvalues rs >>= fun rs ->
               Machine.return (rs @ vs)
             else
               Machine.return vs) >>= fun vs ->
            external_function vs f >>= fun return ->
            (if Options.get_retromips () then (
               let r =
                 `Register
                   (RId (Arch.Mips.(string_of_register return_register)))
               in
               assign r return
             )
             else Machine.return ()) >>= fun () ->
            Machine.return return
        )
    | _ ->
        assert false

  and external_function vs (FId f) =
    match f, vs with
    | "allocate_block", (DInt size :: _) ->
        Machine.get >>= fun runtime ->
        let addr =
          Common.Memory.allocate runtime.memory size (DInt Int32.zero)
        in
        Machine.return @@ DLocation addr
    | "write_block", (DLocation location :: DInt i :: v :: _) ->
        Machine.get >>= fun runtime ->
        let block = Common.Memory.dereference runtime.memory location in
        Common.Memory.write block i v;
        Machine.return DUnit
    | "read_block", (DLocation location :: DInt i :: _) ->
        Machine.get >>= fun runtime ->
        let block = Common.Memory.dereference runtime.memory location in
        Machine.return @@ Common.Memory.read block i
    | "print_int", (DInt i :: _) ->
        print_string (Int32.to_string i);
        Machine.return DUnit
    | "print_char", (DChar i :: _) ->
        print_char i;
        Machine.return DUnit
    | "print_string", (DString i :: _) ->
        print_string i;
        Machine.return DUnit
    | _ -> failwith (
      Printf.sprintf
        "NoSuchFunction or InvalidApplication of `%s' \
         (%d argument(s) provided : %s)."
        f
        (List.length vs)
        (String.concat " " (List.map type_of vs))
    )

  and bind_local x v =
    Machine.modify (fun runtime ->
      { runtime with lvariables = IdMap.add x v runtime.lvariables }
    )
  and bind_locals xs vs =
    List.fold_left2 (fun acc x v ->
      acc >>= fun () -> bind_local x v
    ) (Machine.return ()) xs vs
  in
  let extract_observable runtime0 runtime =
    { new_variables =
        IdMap.filter
          (fun x _ -> not
              (IdMap.mem x runtime0.gvariables || is_intermediate x))
          runtime.gvariables
    }
  in
  Machine.get >>= fun runtime0 ->
  program ast >>= fun () ->
  Machine.get >>= fun runtime ->
  Machine.return @@ extract_observable runtime0 runtime

let print_observable runtime obs =
  String.concat "\n" (List.map (fun (Id k, v) ->
    Printf.sprintf "%s = %s" k (print_data runtime.memory v)
  ) (IdMap.bindings obs.new_variables))
