(** A module for undirected graphs.

    This module provides a functional data structure to represent a
    graph which nodes contain a set of labels and which edges can have
    one label too.

    We maintain the invariant that two nodes always have different
    labels: thus, nodes are identified by their labels.

*)

module type EdgeLabelSig = sig
  include Set.OrderedType
  (** [all] enumerates all the possible edge labels. *)
  val all : t list
  (** [to_string e] converts [e] in a human readable value. *)
  val to_string : t -> string
end

module type NodeLabelSig = sig
  include Set.OrderedType
  (** [to_string n] converts [n] in a human readable value. *)
  val to_string : t -> string
end

module Make (EdgeLabel : EdgeLabelSig) (NodeLabel : NodeLabelSig) : sig

  (** The type for graphs. *)
  type t

  (** The empty graph. *)
  val empty : t

  (** [add_node g [n1;...;nN]] returns a new graph that extends [g] with
      a new node labelled by [n1;...;nN]. None of the [nI] can be used
      by another node in [g]. Otherwise, [InvalidNode] is raised.

      In the sequel, the new node can be identified by any [nI].
  *)
  val add_node : t -> NodeLabel.t list -> t
  exception InvalidNode
  exception InvalidEdge

  (** [add_edge g n1 e n2] returns a new graph that extends [g] with a
      new edge between [n1] and [n2]. The edge is labelled by [e]. If [n1]
      or [n2] does not exist, then [InvalidNode] is raised. *)
  val add_edge : t -> NodeLabel.t -> EdgeLabel.t -> NodeLabel.t -> t

  (** [del_edge g n1 e n2] returns a new graph that restricts [g] by removing
      thge edge between [n1] and [n2]. The edge is labelled by [e]. If [n1]
      or [n2] does not exist, then [InvalidNode] is raised. If there is no
      such edge between [n1] and [n2] then [InvalidEdge] is raised. *)
  val del_edge : t -> NodeLabel.t -> EdgeLabel.t -> NodeLabel.t -> t

  (** [del_node g n] returns a new graph that contains [g] minus the
      node [n] and its edges. *)
  val del_node : t -> NodeLabel.t -> t

  (** [neighbours g e n] returns the neighbours of [n] in [g]
      that are connected with an edge labelled by [e]. One neighbour is
      characterized by all its node labels. *)
  val neighbours : t -> EdgeLabel.t -> NodeLabel.t -> NodeLabel.t list list

  (** [neighbours' g [e1;..;eN] n] returns the neighbours of [n] in [g]
      that are connected with edges labelled by [e1; ...; eN]. One neighbour is
      characterized by all its node labels. *)
  val neighbours' :
    t -> EdgeLabel.t list -> NodeLabel.t -> NodeLabel.t list list

  (** [edges g e] returns all the edges of kind [e] in [g].
      WARNING: This function is inefficient! Use it only for debugging. *)
  val edges : t -> EdgeLabel.t -> (NodeLabel.t list * NodeLabel.t list) list

  (** [min_degree g c nc] returns a node of minimal degree for [c]
      that has no edge for [nc], or returns None if no such node exists. *)
  val min_degree : t -> EdgeLabel.t -> EdgeLabel.t -> (int * NodeLabel.t) option

  (** [pick_edge g c] returns an arbitrary edge for [c] or None if
      there is no such edge. *)
  val pick_edge : t -> EdgeLabel.t -> (NodeLabel.t * NodeLabel.t) option

  (** [merge g n1 n2] returns a new graph which is [g] in which [n1]
      and [n2] have been merged. *)
  val merge : t -> NodeLabel.t -> NodeLabel.t -> t

  (** [all_labels g n] returns all the node labels of node [n]. *)
  val all_labels : t -> NodeLabel.t -> NodeLabel.t list

  (** [are_connected g n1 e n2] returns true iff [n1] and [n2] are connected
      by [e]. *)
  val are_connected : t -> NodeLabel.t -> EdgeLabel.t -> NodeLabel.t -> bool

  (** [show g labels] runs [dotty] to display the graph [g]. [labels n] may
      optionally return an additional information to be display in the node
      for [n]. *)
  val show : t -> (NodeLabel.t -> string option) -> unit
  val dump : t -> string

end
