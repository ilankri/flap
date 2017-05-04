(**

   The register allocation translates a Retrolix program into an
   equivalent Retrolix program that uses hardware registers as much as
   possible to hold intermediate results.

   Register allocation is done in two steps:

   - a static analysis called "Liveness Analysis of Variables" is
   performed to compute a graph. It approximates the interference
   relation of program variables, i.e. the intersection between the
   live ranges of variables. The nodes of the graph are the program
   variables and, in this graph, a node for 'x' and a node 'y' are
   connected iff 'x' and 'y' are in interference.

   - a graph coloring algorithm is executed on the interference graph:
   if two variables live at the same time, then their values cannot
   be carried by the same register ; thus, it suffices to use a different
   color for their nodes. Graph coloring is NP-complete. Yet, we will
   use a simple recursive algorithm that provides good results in
   practice.

*)

open RetrolixAST

module LabelOrd = struct
  type t = label
  let compare (Label l1) (Label l2) = String.compare l1 l2
end

module LabelMap = Map.Make (LabelOrd)
module LabelSet = Set.Make (LabelOrd)

(** Liveness Analysis. *)
type location = lvalue

module LSet = Set.Make (struct
    type t = location
    let compare = compare
end)

(**
 * For each IN and OUT, we have a Map in structure :
   LabelOrd -> LSet
 * *)
type liveness_analysis_result = {
  live_use : LSet.t LabelMap.t;
  live_def : LSet.t LabelMap.t;
  live_in  : LSet.t LabelMap.t;
  live_out : LSet.t LabelMap.t;
}

let find_default d k m =
  try LabelMap.find k m with Not_found -> d

let empty_results =
{
  live_use = LabelMap.empty;
  live_def = LabelMap.empty;
  live_in  = LabelMap.empty;
  live_out = LabelMap.empty;
}

let string_of_lvalue = function
  | `Register (RId r) -> "$" ^ r
  | `Variable (Id r) -> r

let string_of_label (Label s) = s

let string_of_lset s =
  String.concat " " (List.map string_of_lvalue (LSet.elements s))

let string_of_lmap m =
  String.concat "\n" (
      List.map (fun (l, s) ->
          Printf.sprintf "  %s : %s\n" (string_of_label l) (string_of_lset s)
      ) (LabelMap.bindings m)
  )

let string_of_results r =
  Printf.sprintf
    "IN:\n%s\nOUT:\n%s\n"
    (string_of_lmap r.live_in)
    (string_of_lmap r.live_out)

(** [def i] returns the variables defined by [i]. *)
let def = function
  | Call (lv, _, _) ->
    (* What about $ra?  *)
    LSet.add (register MipsArch.return_register) (LSet.singleton lv)
  | Assign (lv, _, _) -> LSet.singleton lv
  | TailCall _ | Ret _ | Jump _ | ConditionalJump _ | Switch _ | Comment _ |
    Exit -> LSet.empty

(** [use i] returns the variables used by [i]. *)
let use i =
  let add_rvalue set = function
    | `Immediate _ -> set
    | `Register _ | `Variable _ as rv -> LSet.add rv set
  in
  let rvs =
    match i with
    | Switch (`Immediate _, _, _) | Jump _ | Comment _ | Exit -> []
    | Call (_, rv, rvs) | TailCall (rv, rvs) ->
      rv :: rvs @ List.map register MipsArch.argument_passing_registers
    | Ret rv -> [rv; register MipsArch.return_address_register]
    | Assign (_, _, rvs) | ConditionalJump (_, rvs, _, _) -> rvs
    | Switch (rv, _, _) -> [rv]
  in
  List.fold_left add_rvalue LSet.empty rvs

(** [successors p] returns a function [succ] such that [succ l] returns
    the successors of [l] in the control flow graph. *)
let successors = function
  | DValue (_, (_, instrs)) | DFunction (_, _, (_, instrs)) ->
    fun label -> (
        match List.assoc label instrs with
        | Ret _ | Exit -> LabelSet.empty
        | Jump l -> LabelSet.singleton l
        | ConditionalJump (_, _, l, l') ->
          LabelSet.add l (LabelSet.singleton l')
        | Call _ | TailCall _ | Assign _ | Switch _ | Comment _ ->
          try
            let cur_instr_index =
              ExtStd.List.index_of (fun (l, _) -> l = label) instrs
            in
            let next_label, _ = List.nth instrs (cur_instr_index + 1) in
            LabelSet.singleton next_label
          with
          | Failure "nth" ->    (* [label] has no successor.  *)
            LabelSet.empty
          | Not_found -> assert false
      )
  | DExternalFunction _ -> fun _ -> LabelSet.empty

let rec definition res d =
  let resNow = match d with
  | DValue (_, b) -> block res b
  | DFunction (_, idList, b) -> block res b (* idList inutile??? *)
  | DExternalFunction _ -> res
  in
  compare_liveness_result res resNow d

and compare_liveness_result resPre resNow def =
    if (resPre <> resNow) then definition resNow def else resNow

and block res = function
  (* Here we check from the last element in the list in order to converge faster *)
  | (_, insList) -> List.fold_right instruction insList res

  (* In the instruction, we have to do the following sequence for each
   * labelled_instruction :
   * 1. Find the register used and put them in live_use
   * 2. Find the register def and put them in live_def
   * 3. Calculate live_in : live_use + (live_out - live_def)
   * 4. Calculate live_out: live_in of previous step
   * *)
and instruction (_, ins) res = match ins with
  (** l ← call r (r1, ⋯, rN) *)
  | Call (lvalue, rvalue, rvList) -> failwith "TODO"
  (** tailcall r (r1, ⋯, rN) *)
  | TailCall (rv, rvList) -> failwith "TODO"
  (** ret r *)
  | Ret (rv) -> failwith "TODO"
  (** l ← op r1, ⋯, rN *)
  | Assign (lv, op, rvList) -> failwith "TODO"
  (** jump ℓ *)
  | Jump (l) -> failwith "TODO"
  (** jumpif condition r1, r2 → ℓ1, ℓ2 *)
  | ConditionalJump (c, rvList, l1, l2) -> failwith "TODO"
  (** switch r -> l1, ..., lN orelse l. *)
  | Switch (rv, lArray, lOp) -> failwith "TODO"
  (** ;; comment *)
  | Comment _ -> res
  (** exit *)
  | Exit -> res


(** [liveness_analysis p] returns the liveness analysis of [p].

   This is a data flow analysis which overapproximates the variables
   that are alive before (live-in) and after (live-out) each
   instruction. To do so, we use the following two equations:

   in(n)  = use(n) ∪ (out(n) ∖ def(n))
   out(n) = ⋃_{s ∈ successors (n)} in(s)

   for each node n of the control flow graph, i.e. for each label
   of the program.

   As these equations are mutually recursive, they must be solved
   using a fixpoint.

*)
let rec liveness_analysis p : liveness_analysis_result =
  List.fold_left definition empty_results p

(** Interference graph. *)
(** In the interference graph, there will be two kinds of edges: *)
type relation =
  (** If two variables cannot be represented in the same register
      because their liveness ranges intersect, we say that they are in
      a conflict relation. *)
  | Conflict

  (** If two variables are related by a MOVE instruction, we will try
      to put them in the same register, we say that they are in
      a preference relation. *)
  | Preference

(** The node of the interference graph are lvalues and its edges are
    labelled by [relation]. *)
module IGraphColoring = GraphColoring.Make
  (struct
    type t = relation
    let compare = compare
    let all = [ Conflict; Preference ]
    let preference = Preference
    let conflict = Conflict
    let to_string = function
      | Conflict -> "<>"
      | Preference -> "="
   end)
  (struct
    type t = lvalue
    let compare = compare
    let to_string : t -> string = string_of_lvalue
   end)
  (struct
    type t = register
    let all = MipsArch.(List.map (fun r -> RId (string_of_register' r)) all_registers)
    let cardinal = List.length all
    let to_string (RId r) = r
   end)

module IGraph = IGraphColoring.Graph

type interference_graph = IGraph.t

(**

   To construct the interference graph:

   1. At any non-move instruction that defines variable a (where
   live-out variables are b1, ..., bj) add interference edges (a, b1),
   ..., (a, bj).

   2. At a move instruction a ← c (where variables b1, ..., bj are
   live-out) add interference edges (a, b1), ..., (a, bj) for any bi
   that is not the same as c.

*)
let interference_graph p liveness : interference_graph =
   failwith "Student! This is your job!"

(** Graph coloring. *)

let colorize_graph g =
  IGraphColoring.colorize g

(** Register allocation directed by the graph coloring. *)
let register_allocation coloring p =
  p

(** Putting all together. *)
let translate p =
  let liveness = liveness_analysis p in
  if Options.get_verbose_mode () then RetrolixPrettyPrinter.(
    let get_decoration space m l =
      let s = try LabelMap.find l m with Not_found -> LSet.empty in
      [PPrint.string ("{ " ^ string_of_lset s ^ " }")]
      @ (if space then [PPrint.empty] else [])
    in
    let decorations = {
        pre = get_decoration false liveness.live_in;
        post = get_decoration true liveness.live_out
    }
    in
    let p = to_string (program ~decorations) p in
    Printf.eprintf "Liveness:\n%s\n" p;
  );
  let igraph   = interference_graph p liveness in
  let coloring = colorize_graph igraph in
  register_allocation coloring p
