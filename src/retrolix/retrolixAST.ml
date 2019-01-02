(** The abstract syntax tree for retrolix programs. *)

type literal =
  | LInt of Int32.t
  | LFun of function_identifier
  | LChar of char
  | LString of string

and identifier = Id of string

and label = Label of string

and function_identifier = FId of string

type register = RId of string

type lvalue = [ `Variable of identifier | `Register of register ]

type rvalue = [ lvalue | `Immediate of literal ]

type t = definition list

and definition =
  | DValue     of identifier * block
  (** DValue (x, b) is a block [b] that defines a global variable [x]. *)
  | DFunction  of function_identifier * identifier list * block
  (** DFunction (f, xs, ys, b) is a function definition with formal
      parameters [xs], and block [b]. *)
  | DExternalFunction of function_identifier

and block =
  identifier list * labelled_instruction list
(** a block consists in a list of local variables and a list of
    instructions. *)

and labelled_instruction =
  label * instruction

and instruction =
  | Call of lvalue * rvalue * rvalue list
  (** l ← call r (r1, ⋯, rN) *)
  | TailCall of rvalue * rvalue list
  (** tailcall r (r1, ⋯, rN) *)
  | Ret of rvalue
  (** ret r *)
  | Assign of lvalue * op * rvalue list
  (** l ← op r1, ⋯, rN *)
  | Jump of label
  (** jump ℓ *)
  | ConditionalJump of condition * rvalue list * label * label
  (** jumpif condition r1, r2 → ℓ1, ℓ2 *)
  | Switch of rvalue * label array * label option
  (** switch r -> l1, ..., lN orelse l. *)
  | Comment of string
  (** ;; comment *)
  | Exit
  (** exit *)

and op =
  | Load
  | Add | Mul | Div | Sub
  | Bool of condition

and condition =
  | GT | LT | GTE | LTE | EQ

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)
module IdMap = Map.Make (IdCmp)

(**

   In Retrolix, the toplevel value declarations define global
   variables. The identifiers of these variables must be distinct.

*)
exception GlobalIdentifiersMustBeUnique of identifier

let globals =
  List.fold_left (fun globals -> function
    | DValue (x, _) ->
        if IdSet.mem x globals then
          raise (GlobalIdentifiersMustBeUnique x);
        IdSet.add x globals
    | _ ->
        globals
  ) IdSet.empty


(** Convert a list of Retrolix identifiers to a set of Retrolix
    identifiers.  *)
let idset_of_idlist ids =
  List.fold_left (fun acc id -> IdSet.add id acc) IdSet.empty ids

let local globals instr =
  let local = function
    | `Variable id ->
        if IdSet.mem id globals then IdSet.empty else IdSet.singleton id
    | `Register _ | `Immediate _ -> IdSet.empty
  in
  let ( ++ ) = IdSet.union in
  let locals xs = List.fold_left ( ++ ) IdSet.empty (List.map local xs) in
  match instr with
  | Call (lv, rv, rvs) -> local lv ++ local rv ++ locals rvs
  | TailCall (rv, rvs) -> local rv ++ locals rvs
  | Ret rv -> local rv
  | Assign (lv, _, rvs) -> local lv ++ locals rvs
  | Jump _ | Comment _ | Exit -> IdSet.empty
  | ConditionalJump (_, rvs, _, _) -> locals rvs
  | Switch (rv, _, _) -> local rv

(**
   Every function in Retrolix starts with a declaration
   of local variables. So we need a way to compute the
   local variables of some generated code. This is the
   purpose of the next function:
*)

(** [locals globals b] takes a set of variables [globals] and returns
    the variables use in the list of instructions [b] which are not
    in [globals]. *)
let locals globals b =
  IdSet.elements (
    (List.fold_left IdSet.union IdSet.empty
       (List.map (fun (_, instr) -> local globals instr) b))
  )

(** Convert a MIPS register into a Retrolix register.  *)
let register reg = `Register (RId (MipsArch.string_of_register reg))
