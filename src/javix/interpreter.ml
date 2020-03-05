(** This module implements the interpreter of the Javix programming
    language. *)

open Util.Error
open Ast

let error msg =
  global_error "javix execution" msg

type address = int

type value =
  | VInt of int
  | VBox of int
  | VArray of value array
  | VNil

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBox i -> "<"^string_of_int i^">"
  | VArray v ->
      "["^
      (Array.fold_right
         (fun v s -> string_of_value v ^ (if s="" then "" else ";"^s)) v "")
      ^ "]"
  | VNil -> "."

type runtime =
  { mutable code : instruction array;
    mutable jumptbl : (label * int) list;
    mutable stack : value list;
    mutable vars : value array;
    mutable pc : int;
    mutable time : int }

let string_of_binop = function
  | Add -> "Add"
  | Mul -> "Mul"
  | Div -> "Div"
  | Sub -> "Sub"
  | Rem -> "Rem"

let string_of_cmpop = function
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let string_of_instr = function
  | Box -> "Box"
  | Unbox -> "Unbox"
  | Bipush i -> "Push("^string_of_int i^")"
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Dup -> "Dup"
  | Binop op -> string_of_binop op
  | Astore (Var v) -> "Astore("^string_of_int v^")"
  | Aload (Var v) -> "Aload("^string_of_int v^")"
  | Goto (Label s) -> "Goto("^s^")"
  | If_icmp (op,Label s) -> "If("^string_of_cmpop op^","^s^")"
  | Anewarray -> "Anewarray"
  | AAstore -> "AAstore"
  | AAload -> "AAload"
  | Ireturn -> "Ireturn"
  | Comment msg -> "Comment "^msg
  | Tableswitch _ -> "Switch"
  | Checkarray -> "Checkarray"
  | Print s ->
      let p = ref "" in
      String.iter (fun c -> match c with
        | '\\' -> p := !p ^ "\\\\"
        | '\n' -> p := !p ^ "\\n"
        | '\t' -> p := !p ^ "\\t"
        | '"'  -> p := !p ^ "\\\""
        | _    -> p := !p ^ (String.make 1 c)) s;
      "Print \"" ^ !p ^ "\""

let rec string_of_stack i l =
  if i=0 then "..."
  else
    match l with
    | [] -> ""
    | [v] -> string_of_value v
    | v::l -> string_of_stack (i-1) l ^","^ string_of_value v

let string_of_vars a =
  let s = ref "" in
  for i = Array.length a - 1 downto 0 do
    if a.(i) <> VNil then
      s := "v"^string_of_int i^"="^string_of_value a.(i)^" "^ !s
  done;
  !s

let string_of_runtime r =
  Printf.sprintf
    " stk:%s\n %s\ntime:%d pc:%d %s"
    (string_of_stack 10 r.stack)
    (string_of_vars r.vars)
    r.time
    r.pc
    (string_of_instr (r.code.(r.pc)))

type observable = int

module Machine = Util.StateMonad.Make (struct type t = runtime end)

let initial_runtime () =
  { code = [||];
    jumptbl = [];
    stack = [];
    vars = [||];
    pc = 0;
    time = 0 }

let rec evaluate (ast : t) =
  let open Machine.Infix in
  Machine.modify (fun runtime ->
    runtime.code <- Array.of_list (List.map snd ast.code);
    List.iteri (fun i (labo,_) ->
      match labo with
      | Some lab -> runtime.jumptbl <- (lab,i)::runtime.jumptbl
      | None -> ())
      ast.code;
    runtime.vars <- Array.make ast.varsize VNil;
    runtime) >>=
  interp

and interp () =
  let open Machine.Infix in
  Machine.modify (fun r ->
    assert (0 <= r.pc && r.pc < Array.length r.code);
    if Options.get_verbose_mode () then
      (print_string ((string_of_runtime r)^"\n"); flush_all ());
    r.time <- r.time + 1;
    r) >>= fun () ->
  Machine.get >>= fun r ->
  match r.code.(r.pc) with
  | Box ->
      pop_int "Box" >>= fun i -> push (VBox i) >>= next
  | Unbox ->
      pop "Unbox" >>= (function
        | VBox i -> push (VInt i) >>= next
        | _ -> failwith "Incorrect stack head for Unbox")
  | Bipush i -> push (VInt i) >>= next
  | Pop -> pop "Pop" >>= fun _ -> next ()
  | Swap ->
      pop "Swap" >>= fun v2 ->
      pop "Swap" >>= fun v1 ->
      push v2 >>= fun () ->
      push v1 >>=
      next
  | Dup ->
      pop "Dup" >>= fun v ->
      push v >>= fun () ->
      push v >>=
      next
  | Binop op ->
      pop_int "Binop" >>= fun i2 ->
      pop_int "Binop" >>= fun i1 ->
      push (VInt (binop op i1 i2)) >>=
      next
  | Astore (Var var) ->
      pop "Astore" >>= (function
        | VInt _ -> failwith "Astore on a non-boxed integer"
        | v -> Machine.modify (fun r -> r.vars.(var) <- v; r) >>= next)
  | Aload (Var var) -> push (r.vars.(var)) >>= next
  | Goto lab -> goto lab
  | If_icmp (op, lab) ->
      pop_int "If_icmp" >>= fun i2 ->
      pop_int "If_icmp" >>= fun i1 ->
      if cmpop op i1 i2 then goto lab else next ()
  | Anewarray ->
      pop_int "Anewarray" >>= fun i ->
      push (VArray (Array.make i VNil)) >>=
      next
  | AAstore ->
      pop "AAstore" >>= fun v ->
      pop_int "AAstore" >>= fun i ->
      pop "AAstore" >>= fun a ->
      (match v,a with
       | VInt _, _ -> failwith "AAstore of a non-boxed integer"
       | _,VArray a -> a.(i) <- v; next ()
       | _ -> failwith "AAstore on a non-VArray")
  | AAload ->
      pop_int "AAstore" >>= fun i ->
      pop "AAstore" >>= fun a ->
      (match a with
       | VArray a -> push a.(i) >>= next
       | _ -> failwith "AAload on a non-VArray")
  | Ireturn ->
      pop_int "Ireturn" >|= fun i ->
      if r.stack <> []
      then print_string "Warning: Ireturn discards some stack\n";
      i
  | Comment _ -> next ()
  | Tableswitch (n, labs, lab) ->
      pop_int "Tableswitch" >>= fun i ->
      if 0 <= i-n && i-n < List.length labs then goto (List.nth labs (i-n))
      else goto lab
  | Checkarray ->
      pop "Checkarray" >>= fun a ->
      (match a with
       | VArray _ -> push a >>= next
       | _ -> failwith "Checkarray on a non-VArray")
  | Print s -> Printf.printf "%s" s; push (VInt 0) >>= next

and next () =
  let open Machine.Infix in
  Machine.modify (fun r -> r.pc <- r.pc + 1; r) >>= interp

and goto lab =
  let open Machine.Infix in
  Machine.modify (fun r -> r.pc <- List.assoc lab r.jumptbl; r) >>= interp

and pop msg =
  let open Machine.Infix in
  Machine.get >>= fun r ->
  match r.stack with
  | v :: l -> Machine.modify (fun r -> r.stack <- l; r) >|= fun () -> v
  | [] -> failwith ("Not enough stack for "^msg)

and pop_int msg =
  let open Machine.Infix in
  pop msg >>= function
  | VInt i -> Machine.return i
  | _ -> failwith ("Incorrect stack head for "^msg)

and push v = Machine.modify (fun r -> r.stack <- v :: r.stack; r)

and binop = function
  | Add -> (+)
  | Mul -> ( * )
  | Div -> (/)
  | Sub -> (-)
  | Rem -> (mod)

and cmpop = function
  | Eq -> (=)
  | Ne -> (<>)
  | Lt -> (<)
  | Le -> (<=)
  | Gt -> (>)
  | Ge -> (>=)

let print_observable _ obs = string_of_int obs
