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

module LabelMap = Map.Make (struct
  type t = label
  let compare = compare
end)

(** Liveness Analysis. *)
type location = lvalue

module LSet = Set.Make (struct
    type t = location
    let compare = compare
end)

type liveness_analysis_result = {
  live_in  : LSet.t LabelMap.t;
  live_out : LSet.t LabelMap.t;
}

let find_default d k m =
  try LabelMap.find k m with Not_found -> d

let empty_results =
  {
    live_in = LabelMap.empty;
    live_out = LabelMap.empty;
  }

(** [def i] returns the variables defined by [i]. *)
let def i =
  failwith "Student! This is your job!"

(** [use i] returns the variables defined by [i]. *)
let use i =
  failwith "Student! This is your job!"

(** [predecessors p] returns a function [p] such that [p l] returns
    the predecessors of [l] in the control flow graph. *)
let predecessors p =
  failwith "Student! This is your job!"

(** [liveness_analysis p] returns the liveness analysis of [p]. *)
let liveness_analysis p =
  ()

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
    let to_string : t -> string = function
      | `Register (RId r) -> "$" ^ r
      | `Variable (Id r) -> r
   end)
  (struct
    type t = register
    (** The following will change when we will seriously implement the MIPS backend.  *)
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
  ()

(** Register allocation directed by the graph coloring. *)
let register_allocation coloring p =
  p

(** Putting all together. *)
let translate p =
  let liveness = liveness_analysis p in
  let igraph   = interference_graph p liveness in
  let coloring = colorize_graph igraph in
  register_allocation coloring p
