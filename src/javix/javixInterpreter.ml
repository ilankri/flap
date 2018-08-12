(** This module implements the interpreter of the Javix programming
    language. *)

open Error
open JavixAST

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
     "["^ (Array.fold_right
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

let initial_runtime () =
  { code = [||];
    jumptbl = [];
    stack = [];
    vars = [||];
    pc = 0;
    time = 0 }

let rec evaluate runtime (ast : t) =
  runtime.code <- Array.of_list (List.map snd ast.code);
  List.iteri (fun i (labo,_) ->
              match labo with
              | Some lab -> runtime.jumptbl <- (lab,i)::runtime.jumptbl
              | None -> ())
             ast.code;
  runtime.vars <- Array.make ast.varsize VNil;
  let ret = interp runtime in
  runtime, ret

and interp r =
  assert (0 <= r.pc && r.pc < Array.length r.code);
  if Options.get_verbose_mode () then
    (print_string ((string_of_runtime r)^"\n"); flush_all ());
  r.time <- r.time + 1;
  match r.code.(r.pc) with
  | Box ->
     let i = pop_int r "Box" in push (VBox i) r; next r
  | Unbox ->
     (match pop r "Unbox" with
      | VBox i -> push (VInt i) r; next r
      | _ -> failwith "Incorrect stack head for Unbox")
  | Bipush i -> push (VInt i) r; next r
  | Pop -> let _ = pop r "Pop" in next r
  | Swap ->
     let v2 = pop r "Swap" in
     let v1 = pop r "Swap" in
     push v2 r; push v1 r; next r
  | Dup ->
     let v = pop r "Dup" in
     push v r; push v r; next r
  | Binop op ->
     let i2 = pop_int r "Binop" in
     let i1 = pop_int r "Binop" in
     push (VInt (binop op i1 i2)) r; next r
  | Astore (Var var) ->
     (match pop r "Astore" with
      | VInt _ -> failwith "Astore on a non-boxed integer"
      | v -> r.vars.(var) <- v; next r)
  | Aload (Var var) -> push (r.vars.(var)) r; next r
  | Goto lab -> goto lab r
  | If_icmp (op, lab) ->
     let i2 = pop_int r "If_icmp" in
     let i1 = pop_int r "If_icmp" in
     if cmpop op i1 i2 then goto lab r else next r
  | Anewarray ->
     let i = pop_int r "Anewarray" in
     push (VArray (Array.make i VNil)) r; next r
  | AAstore ->
     let v = pop r "AAstore" in
     let i = pop_int r "AAstore" in
     let a = pop r "AAstore" in
     (match v,a with
      | VInt _, _ -> failwith "AAstore of a non-boxed integer"
      | _,VArray a -> a.(i) <- v; next r
      | _ -> failwith "AAstore on a non-VArray")
  | AAload ->
     let i = pop_int r "AAstore" in
     let a = pop r "AAstore" in
     (match a with
      | VArray a -> push a.(i) r; next r
      | _ -> failwith "AAload on a non-VArray")
  | Ireturn ->
     let i = pop_int r "Ireturn" in
     if r.stack <> []
     then print_string "Warning: Ireturn discards some stack\n";
     i
  | Comment _ -> next r
  | Tableswitch (n, labs, lab) ->
     let i = pop_int r "Tableswitch" in
     if 0 <= i-n && i-n < List.length labs then goto (List.nth labs (i-n)) r
     else goto lab r
  | Checkarray ->
     let a = pop r "Checkarray" in
     (match a with
      | VArray _ -> push a r; next r
      | _ -> failwith "Checkarray on a non-VArray")
  | Print s -> Printf.printf "%s" s; push (VInt 0) r; next r

and next r = r.pc <- r.pc + 1; interp r

and goto lab r = r.pc <- List.assoc lab r.jumptbl; interp r

and pop r msg =
  match r.stack with
  | v :: l -> r.stack <- l; v
  | [] -> failwith ("Not enough stack for "^msg)

and pop_int r msg =
  match pop r msg with
  | VInt i -> i
  | _ -> failwith ("Incorrect stack head for "^msg)

and push v r = r.stack <- v :: r.stack

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

let print_observable runtime obs = string_of_int obs
