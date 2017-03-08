(** This module implements the interpreter of the Retrolix programming
    language. *)

open Error
open RetrolixAST

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
  | DLocation of Memory.location
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
    let b = Memory.dereference m a in
    let vs = Array.to_list (Memory.array_of_block b) in
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
  mutable memory : data Memory.t;
  functions      : function_definition FIdMap.t
}

and function_definition = {
  formals : identifier list;
  body : block;
}

type observable = {
  new_variables : data IdMap.t
}

let initial_runtime () = {
  return     = None;
  gvariables = IdMap.empty;
  lvariables = IdMap.empty;
  registers  = RIdMap.empty;
  memory     = Memory.create (640 * 1024);
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

let evaluate runtime0 (ast : t) =
  let extract_function_definition runtime = function
    | DValue _ -> runtime
    | DFunction (f, formals, body) ->
      { runtime with functions =
          FIdMap.add f { formals; body } runtime.functions
      }
    | DExternalFunction f ->
      runtime
  in
  let rec program runtime ds =
    let runtime = List.fold_left extract_function_definition runtime ds in
    List.fold_left definition runtime ds
  and definition runtime = function
    | DValue (x, b) ->
      let runtime = block runtime b in
      begin match runtime.return with
	| None ->
	  runtime
	| Some v ->
	  { runtime with gvariables = IdMap.add x v runtime.gvariables }
      end
    | DFunction (f, xs, b) ->
      runtime
    | DExternalFunction f ->
      runtime
  and block runtime b =
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
    let locals0 = runtime.lvariables in
    let locals = fst b in
    let start = Hashtbl.find jump_table (fst (List.hd (snd b))) in
    let runtime = List.fold_left (fun r x -> bind_local r x (DInt Int32.zero)) runtime locals in
    let runtime = instruction runtime jump_table start in
    { runtime with lvariables = locals0 }

  and instruction runtime jump_table (i, next) =
    let jump l runtime =
      try
        instruction runtime jump_table (Hashtbl.find jump_table l)
      with Not_found ->
        let Label l = l in
        failwith (Printf.sprintf "Label %s not found" l)
    in
    let continue runtime =
      match next with
        | None -> runtime
        | Some l -> jump l runtime
    in
    match i with
      | Call (out, `Immediate (LFun (FId "block_create")), [ r1 ]) ->
         assign runtime out (match rvalue runtime r1 with
             | DInt size ->
	        let addr = Memory.allocate runtime.memory size (DInt Int32.zero) in
                DLocation addr
             | _ -> assert false) |> continue
      | Call (out, `Immediate (LFun (FId "block_set")), rs) ->
         assign runtime out (match List.map (rvalue runtime) rs with
                       [ DLocation location; DInt i; v ] ->
                       let block = Memory.dereference runtime.memory location in
                       Memory.write block i v;
                       DUnit
                     | _ -> assert false) |> continue
      | Call (out, `Immediate (LFun (FId "block_get")), rs) ->
         assign runtime out (match List.map (rvalue runtime) rs with
                     | [ DLocation location; DInt i ] ->
                        let block = Memory.dereference runtime.memory location in
                        Memory.read block i
                     | _ -> assert false) |> continue
      | Call (x, f, rs) ->
        let y, runtime = call runtime (rvalue runtime f) (List.map (rvalue runtime) rs) in
        assign runtime x y |> continue
      | TailCall (f, rs) ->
        let _, runtime = call runtime (rvalue runtime f) (List.map (rvalue runtime) rs) in
        continue runtime
      | Ret r ->
        { runtime with return = Some (rvalue runtime r) }
      | Assign (x, o, rs) ->
        assign runtime x (op runtime o (List.map (rvalue runtime) rs)) |> continue
      | Jump l ->
        jump l runtime
      | ConditionalJump (c, rs, l1, l2) ->
        if condition c (List.map (rvalue runtime) rs) then
          jump l1 runtime
        else
          jump l2 runtime
      | Comment _ ->
        continue runtime
      | Switch (r, ls, default) ->
	begin match rvalue runtime r with
	  | DInt x ->
	    let x = Int32.to_int x in
	    if  x < Array.length ls then
	      jump ls.(x) runtime
	    else
	      begin match default with
		| None -> failwith "Non exhaustive switch."
		| Some l -> jump l runtime
	      end
	  | _ ->
	    assert false (* By typing. *)
	end
      | Exit ->
        runtime
  and rvalue runtime = function
    | `Variable x ->
      (try
         IdMap.find x runtime.lvariables
       with Not_found ->
         (try
            IdMap.find x runtime.gvariables
          with Not_found ->
            let Id x = x in
            failwith (Printf.sprintf "Variable %s not found" x)
         )
      )
    | `Register x ->
      (try
         RIdMap.find x runtime.registers
       with Not_found ->
         DInt Int32.zero
      )
    | `Immediate l ->
      literal l
  and op runtime o vs =
    match o, vs with
      | Load, [ v ] -> v
      | Add, [ DInt x; DInt y ] ->
        DInt (Int32.add x y)
      | Mul, [ DInt x; DInt y ] ->
        DInt (Int32.mul x y)
      | Div, [ DInt x; DInt y ] ->
        DInt (Int32.div x y)
      | Sub, [ DInt x; DInt y ] ->
        DInt (Int32.sub x y)
      | Bool c, vs ->
        DInt (if condition c vs then Int32.one else Int32.zero)
      | _, _ ->
        assert false

  and condition op vs =
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

  and assign runtime lvalue v =
    match lvalue with
      | `Variable x ->
        if IdMap.mem x runtime.lvariables then
          { runtime with lvariables = IdMap.add x v runtime.lvariables }
        else
          { runtime with gvariables = IdMap.add x v runtime.gvariables }
      | `Register x ->
        { runtime with registers = RIdMap.add x v runtime.registers }

  and call runtime fv vs =
    match fv with
      | DFun f ->
        (try
           let fdef = FIdMap.find f runtime.functions in
           let runtime = List.fold_left2 bind_local runtime fdef.formals vs in
           let runtime = block runtime fdef.body in
           let return = match runtime.return with None -> assert false | Some x -> x in
           (return, runtime)
         with Not_found ->
           external_function runtime vs f
        )
      | _ ->
        assert false

  and external_function runtime vs (FId f) =
    match f with
      | _ -> failwith ("NoSuchFunction: " ^ f)

  and bind_local runtime x v =
    { runtime with lvariables = IdMap.add x v runtime.lvariables }
  in
  let extract_observable runtime =
    { new_variables =
        IdMap.filter
          (fun x _ -> not (IdMap.mem x runtime0.gvariables || is_intermediate x))
          runtime.gvariables
    }
  in
  let runtime = program runtime0 ast in
  let observable = extract_observable runtime in
  (runtime, observable)

let print_observable runtime obs =
  String.concat "\n" (List.map (fun (Id k, v) ->
    Printf.sprintf "%s = %s" k (print_data runtime.memory v)
  ) (IdMap.bindings obs.new_variables))
