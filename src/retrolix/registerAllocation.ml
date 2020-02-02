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
     if two variables live at the same time, then their values cannot be
     carried by the same register ; thus, it suffices to use a different
     color for their nodes. Graph coloring is NP-complete. Yet, we will
     use a simple recursive algorithm that provides good results in
     practice.

*)

open Ast

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

type liveness_analysis_result =
  {
    (* live_def and live_use are here for quick reference only for calculation.
     * When compare equal, we don't compare live_def and live_use *)
    live_def : LSet.t LabelMap.t;
    live_use : LSet.t LabelMap.t;
    live_in  : LSet.t LabelMap.t;
    live_out : LSet.t LabelMap.t;
  }

let find_default d k m =
  try LabelMap.find k m with Not_found -> d

let empty_results =
  {
    live_def = LabelMap.empty;
    live_use = LabelMap.empty;
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
      LSet.add (register Arch.Mips.return_register) (LSet.singleton lv)
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
        rv :: rvs @ List.map register Arch.Mips.argument_passing_registers
    | Ret rv -> [rv; register Arch.Mips.return_address_register]
    | Assign (_, _, rvs) | ConditionalJump (_, rvs, _, _) -> rvs
    | Switch (rv, _, _) -> [rv]
  in
  List.fold_left add_rvalue LSet.empty rvs

(** [successors l block] returns a set of labels that curret label l will
 * execute NEXT in the control flow graph. *)
let successors label (_, instrs) =
  match List.assoc label instrs with
  | Ret _ | Exit -> LabelSet.empty
  | Jump l -> LabelSet.singleton l
  | ConditionalJump (_, _, l, l') ->
      LabelSet.add l (LabelSet.singleton l')
  | Switch (_, ls, l) ->
      let acc = match l with
        | None -> LabelSet.empty
        | Some l -> LabelSet.singleton l in
      Array.fold_right LabelSet.add ls acc
  | Call _ | TailCall _ | Assign _ | Comment _ ->
      try
        let cur_instr_index =
          Util.ExtStd.List.index_of (fun (l, _) -> l = label) instrs in
        let next_label, _ = List.nth instrs (cur_instr_index + 1) in
        LabelSet.singleton next_label
      with
      | Failure _ ->            (* [label] has no successor.  *)
          LabelSet.empty
      | Not_found -> assert false

let rec definition res d =
  let resNow = match d with
    | DValue (_, b) -> block res b
    | DFunction (_, _, b) -> block res b
    | DExternalFunction _ -> res
  in
  compare_liveness_result res resNow d

(* Do not use [<>] to compare liveness analysis results.  Use
   instead [equal] functions provided by modules [Map] and [Set].  *)
and compare_liveness_result resPre resNow def =
  let map_eq s1 s2 = LSet.equal s1 s2 in
  let compare_result_eq pre now =
    (let inIsEqual = LabelMap.equal map_eq pre.live_in now.live_in in
     let outIsEqual = LabelMap.equal map_eq pre.live_out now.live_out in
     inIsEqual && outIsEqual) in
  if (compare_result_eq resPre resNow) then resNow else definition resNow def

and block res = function
  (* Here we check from the last element in the list in order to
     converge faster *)
  | (_, insList) as b ->
      let res' = List.fold_right prepare_def_and_use insList res
      in List.fold_right (instruction b) insList res'

(* In the instruction, we have to do the following sequence for each
 * labelled_instruction :
 * 1. Find the register def and put them in live_def
 * 2. Find the register used and put them in live_use
 * 3. Calculate live_out: live_in of successors
 * 4. Calculate live_in : live_use + (live_out - live_def)
 * *)
and prepare_def_and_use (l, ins) res =
  let live_def' = LabelMap.add l (def ins) res.live_def in
  let live_use' = LabelMap.add l (use ins) res.live_use in
  { res with live_def = live_def'; live_use = live_use' }

and instruction b (l, _ins) res =
  let locationOutSet = calc_out l b res in
  let live_out' = LabelMap.add l locationOutSet res.live_out in
  let res' = { res with live_out = live_out' } in
  let locationInSet = calc_in l res' in
  let live_in' = LabelMap.add l locationInSet res.live_in in
  { res' with live_in = live_in' }

and location_set_union liveMap l lSet =
  try
    LSet.union lSet (LabelMap.find l liveMap)
  with
  | Not_found -> lSet

and calc_out l b res =
  let succ = successors l b in
  if (LabelSet.cardinal succ) > 0
  then
    LabelSet.fold (location_set_union res.live_out) succ LSet.empty
  else
    LSet.empty

and calc_in l res =
  let outSet = location_set_union res.live_out l LSet.empty in
  let defSet = location_set_union res.live_def l LSet.empty in
  let useSet = location_set_union res.live_use l LSet.empty in
  LSet.union useSet (LSet.diff outSet defSet)

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
let liveness_analysis p : liveness_analysis_result =
  let {live_in; live_out} = List.fold_left definition empty_results p in
  let globals = globals p in
  let exclude_globals lmap =
    LabelMap.map (fun l ->
      LSet.filter (function
        | `Register _ -> true
        | `Variable id -> not @@ IdSet.mem id globals
      ) l
    ) lmap
  in
  {
    live_in = exclude_globals live_in;
    live_out = exclude_globals live_out;
    live_def = Util.ExtStd.failwith_todo __LOC__;
    live_use = Util.ExtStd.failwith_todo __LOC__
  }

(* Interference graph. *)

(* In the interference graph, there will be two kinds of edges: *)
type relation =
  (* If two variables cannot be represented in the same register
      because their liveness ranges intersect, we say that they are in
      a conflict relation. *)
  | Conflict

  (* If two variables are related by a MOVE instruction, we will try
      to put them in the same register, we say that they are in
      a preference relation. *)
  | Preference

(* The node of the interference graph are lvalues and its edges are
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
      let all =
        Arch.Mips.(
          List.map (fun r -> RId (string_of_register' r)) all_registers
        )
      let cardinal = List.length all
      let to_string (RId r) = r
    end)

module IGraph = IGraphColoring.Graph

type interference_graph = IGraph.t

(** Extend [igraph] with [var] and its relations with the variables in
    [live_out_vars] via the function [relate].  *)
let add_var igraph var live_out_vars relate =
  LSet.fold (fun live_out_var igraph ->
    let igraph = IGraph.add_node igraph [live_out_var] in
    let rel = relate var live_out_var in
    IGraph.add_edge igraph var rel live_out_var
  ) live_out_vars (IGraph.add_node igraph [var])

(**

   To construct the interference graph:

   1. At any non-move instruction that defines variable a (where
   live-out variables are b1, ..., bj) add interference edges (a, b1),
   ..., (a, bj).

   2. At a move instruction a ← c (where variables b1, ..., bj are
   live-out) add interference edges (a, b1), ..., (a, bj) for any bi
   that is not the same as c.

*)
let add_relations igraph (label, instr) live_out =
  let live_out_vars = LabelMap.find label live_out in
  match instr with
  | Assign (var, Load, [var']) ->
      let relate _ live_out_var =
        match var' with
        | `Immediate _ -> Conflict
        | `Variable _ | `Register _ ->
            let as_lvalue = function
              | `Immediate _ -> assert false
              | `Register _ | `Variable _ as rv -> rv
            in
            if as_lvalue var' = live_out_var then Preference else Conflict
      in
      add_var igraph var live_out_vars relate
  | Assign _ | Call _ | TailCall _ | Ret _ | Jump _ | ConditionalJump _ |
    Switch _ | Comment _ | Exit as instr ->
      LSet.fold (fun var igraph ->
        add_var igraph var live_out_vars (fun _ _ -> Conflict)
      ) (def instr) igraph

let interference_graph p liveness : interference_graph =
  let aux igraph instrs live_out =
    List.fold_left (fun igraph instr ->
      add_relations igraph instr live_out
    ) igraph instrs
  in
  List.fold_left (fun igraph def ->
    match def with
    | DValue (_, (_, instrs)) | DFunction (_, _, (_, instrs)) ->
        aux igraph instrs liveness.live_out
    | DExternalFunction _ -> igraph
  ) IGraph.empty p

(** Graph coloring. *)

let colorize_graph g =
  IGraphColoring.colorize g

(** Register allocation directed by the graph coloring. *)
let register_allocation _coloring p =
  p

(** Putting all together. *)
let translate p =
  let liveness = liveness_analysis p in
  if Options.get_verbose_mode () then PrettyPrinter.(
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
