(** A module for undirected graphs.

    The implementation is based on purely functional datastructures
    only: we make use of OCaml's standard modules Map and Set.

    The implementation is generic with respect to the type of labels
    for nodes and for edges. The module is therefore a functor
    parameterized by the two descriptions of these types and their
    operations.

*)

(** The type for edge labels.

    We assume that there is a relatively small number of edge labels.
    These labels are comparable and enumerable.

*)
module type EdgeLabelSig = sig
  include Set.OrderedType

  val all : t list
  (** [all] enumerates all the possible edge labels. *)

  val to_string : t -> string
  (** [to_string e] converts [e] in a human readable value. *)
end

(** The type for node labels.

    Node labels must be comparable. *)
module type NodeLabelSig = sig
  include Set.OrderedType

  val to_string : t -> string
  (** [to_string n] converts [n] in a human readable value. *)
end

(** The functor is parameterized by the previous two signatures. *)
module Make (EdgeLabel : EdgeLabelSig) (NodeLabel : NodeLabelSig) =
struct

  (** A type for maps whose keys are integers. *)
  module IntMap = Map.Make (struct type t = int let compare = compare end)
  let int_map_update k m d f = ExtStd.update IntMap.find IntMap.add k m d f

  (** A type for maps whose keys are edge labels. *)
  module EdgeLabelMap = Map.Make (EdgeLabel)

  (** A type for maps whose keys are node labels. *)
  module NodeLabelMap = Map.Make (NodeLabel)

  (** Internally, each node has an identifier which is a small integer. *)
  type nodeid = NodeId of int
  module IdCmp = struct type t = nodeid let compare = compare end

  (** A type for maps whose keys are node identifiers. *)
  module NodeIdMap = Map.Make (IdCmp)
  let nodeid_map_update k m d f =
    ExtStd.update NodeIdMap.find NodeIdMap.add k m d f

  (** A type for sets of node identifiers. *)
  module NodeIdSet = Set.Make (IdCmp)

  (** The type for graphs.

      The datastructure maintains redundant information about the
      graph using multiple maps. Each map provides a logarithmic
      complexity for important services of the datastructure, namely
      the computation of nodes of least [degrees] and the computation
      of a node [neighbourhood].

      A node is externally characterized by a list of node labels
      while internally it is characterized by a node identifier. We
      also maintain this mapping using maps, namely [node_of_label]
      and [labels].

      [next_node_id] is a counter that helps determining the identifier
      for a newly created node.
  *)

  type t = {
    next_node_id   : int;
    node_of_label  : nodeid NodeLabelMap.t;
    labels         : NodeLabel.t list NodeIdMap.t;
    neighbours     : NodeIdSet.t NodeIdMap.t EdgeLabelMap.t;
    degrees        : NodeIdSet.t IntMap.t EdgeLabelMap.t
  }

  let string_of_nodeid (NodeId x) = string_of_int x

  (** [dump g] returns a text-based representation of the graph, for
      debugging. *)
  let dump g =
    let neighbours =
      EdgeLabelMap.bindings g.neighbours |> List.map (fun (c, m) ->
        NodeIdMap.bindings m |> List.map (fun (id, ids) ->
          Printf.sprintf "%s -%s-> %s"
            (string_of_nodeid id)
            (EdgeLabel.to_string c)
            (String.concat ","
               (List.map string_of_nodeid (NodeIdSet.elements ids)))
        )
      ) |> List.flatten |> String.concat "\n"
    in
    let degrees =
      EdgeLabelMap.bindings g.degrees |> List.map (fun (c, m) ->
        IntMap.bindings m |> List.map (fun (d, ids) ->
          Printf.sprintf "%d =%s=> %s"
            d
            (EdgeLabel.to_string c)
            (String.concat ","
               (List.map string_of_nodeid (NodeIdSet.elements ids)))
        )
      ) |> List.flatten |> String.concat "\n"
    in
    Printf.sprintf "%s\n%s\n" neighbours degrees

  (** The empty graph. *)
  let empty =
    let degrees =
      List.fold_left
        (fun m e -> EdgeLabelMap.add e IntMap.empty m)
        EdgeLabelMap.empty
        EdgeLabel.all
    in
    let neighbours =
      List.fold_left
        (fun m e -> EdgeLabelMap.add e NodeIdMap.empty m)
        EdgeLabelMap.empty
        EdgeLabel.all
    in
    {
      next_node_id = 0;
      node_of_label = NodeLabelMap.empty;
      labels = NodeIdMap.empty;
      neighbours;
      degrees;
    }

  exception InvalidNode
  exception InvalidEdge

  let defined_node g n = NodeLabelMap.mem n g.node_of_label

  let id_of_node g n = NodeLabelMap.find n g.node_of_label

  let nodes_of_id g n = NodeIdMap.find n g.labels

  (** Sanity check for the data structure. *)
  let sanity_check = false
  exception InconsistentDegree
  let check_consistent_degree g =
    (* The number of neighbours of [x] is the degree of [x]. *)
    let valid_degree c x ngb =
      let xdegree = NodeIdSet.cardinal ngb in
      NodeIdSet.mem x (IntMap.find xdegree (EdgeLabelMap.find c g.degrees))
    in
    EdgeLabelMap.iter (fun c ngbs ->
      NodeIdMap.iter (fun x ngb ->
        if not (valid_degree c x ngb) then (
          raise InconsistentDegree
        )
      ) ngbs
    ) g.neighbours

  let update_neighbour update_set g id1 e id2 =
    (* We focus on [e]. *)
    let nbg = EdgeLabelMap.find e g.neighbours
    and deg = EdgeLabelMap.find e g.degrees in

    (* What is the degree of id1? *)
    let id1_nbg = try NodeIdMap.find id1 nbg with _ -> assert false in
    let id1_deg = NodeIdSet.cardinal id1_nbg in

    (* Update the neighbours of id1 with update_set id2. *)
    let nbg = nodeid_map_update id1 nbg NodeIdSet.empty (update_set id2) in

    (* Update the degree of id1. *)
    let deg =
      int_map_update id1_deg deg NodeIdSet.empty (NodeIdSet.remove id1)
    in
    let deg =
      if IntMap.find id1_deg deg = NodeIdSet.empty then
        IntMap.remove id1_deg deg
      else
        deg
    in
    let id1_nbg = try NodeIdMap.find id1 nbg with _ -> assert false in
    let id1_deg = NodeIdSet.cardinal id1_nbg in
    let deg = int_map_update id1_deg deg NodeIdSet.empty (NodeIdSet.add id1) in

    (* Finally, update the graph. *)
    let neighbours = EdgeLabelMap.add e nbg g.neighbours
    and degrees = EdgeLabelMap.add e deg g.degrees in
    let g = { g with neighbours; degrees } in

    (* If you suspect a bug in the implementation of the graph data
       structure, which is always possible. Activating sanity check
       might help you to track it down. *)
    if sanity_check then check_consistent_degree g;
    g

  let add_neighbour = update_neighbour NodeIdSet.add
  let del_neighbour = update_neighbour NodeIdSet.remove

  (** [add_node g [n1;...;nN]] returns a new graph that extends [g] with
      a new node labelled by [n1;...;nN]. None of the [nI] can be used
      by another node in [g]. Otherwise, [InvalidNode] is raised.

      In the sequel, the new node can be identified by any [nI].
  *)
  let add_node g ns =
    (* First, a fresh identifier for the node. *)
    let nodeid = NodeId g.next_node_id in
    let next_node_id = g.next_node_id + 1 in

    (* Second, we check that [ns] are not used by any other node. *)
    if List.exists (defined_node g) ns then
      raise InvalidNode;

    (* Third, update maps. *)
    let node_of_label =
      List.fold_left (fun m n -> NodeLabelMap.add n nodeid m) g.node_of_label ns
    in
    let labels = NodeIdMap.add nodeid ns g.labels in
    let neighbours =
      EdgeLabelMap.map (fun nbg ->
        NodeIdMap.add nodeid NodeIdSet.empty nbg
      ) g.neighbours
    in
    (* Initially, the node has a degree 0 since it has no neighbour. *)
    let degrees =
      EdgeLabelMap.map
        (fun deg -> int_map_update 0 deg NodeIdSet.empty (NodeIdSet.add nodeid))
        g.degrees
    in
    { next_node_id; node_of_label; labels; degrees; neighbours }

  (** [add_edge g n1 e n2] returns a new graph that extends [g] with a
      new edge between [n1] and [n2]. The edge is labelled by [e]. If [n1]
      or [n2] does not exist, then [InvalidNode] is raised. *)
  let add_edge g n1 e n2 =
    if not (defined_node g n1 && defined_node g n2) then
      raise InvalidNode;
    let id1 = id_of_node g n1 and id2 = id_of_node g n2 in
    let g = add_neighbour g id1 e id2 in
    let g = add_neighbour g id2 e id1 in
    g

  (** [neighbours g e n] returns the neighbours of [n] in [g]. *)
  let neighbours g e n =
    let id = id_of_node g n in
    let ids =
      NodeIdSet.elements (NodeIdMap.find id (EdgeLabelMap.find e g.neighbours))
    in
    List.map (fun id -> NodeIdMap.find id g.labels) ids

  (** [neighbours' g es n] *)
  let neighbours' g es n =
    let id = id_of_node g n in
    let ids =
      List.map
        (fun e -> NodeIdMap.find id (EdgeLabelMap.find e g.neighbours)) es
    in
    let rec aux = function
      | [] -> assert false
      | [s] -> s
      | s :: ss -> NodeIdSet.inter s (aux ss)
    in
    List.map
      (fun id -> NodeIdMap.find id g.labels) (NodeIdSet.elements (aux ids))

  (** [del_node g n] returns a new graph that contains [g] minus the
      node [n] and its edges. *)
  let del_node g n =
    let id = id_of_node g n in
    let g =
      EdgeLabelMap.fold (fun e nbg g ->
        let nnbg = NodeIdMap.find id nbg in
        NodeIdSet.fold (fun id' g ->
          let g = del_neighbour g id' e id in
          let g = del_neighbour g id e id' in
          g
        ) nnbg g
      ) g.neighbours g
    in
    let neighbours =
      EdgeLabelMap.map (fun nbg ->
        NodeIdMap.remove id nbg
      ) g.neighbours
    in
    let degrees =
      EdgeLabelMap.map (fun deg ->
        let deg0 = IntMap.find 0 deg in
        let deg0 = NodeIdSet.remove id deg0 in
        if deg0 = NodeIdSet.empty then
          IntMap.remove 0 deg
        else
          IntMap.add 0 deg0 deg
      ) g.degrees
    in
    let node_of_label = List.fold_left (fun node_of_label l ->
      NodeLabelMap.remove l node_of_label
    ) g.node_of_label (NodeIdMap.find id g.labels)
    in
    let labels = NodeIdMap.remove id g.labels in
    { g with node_of_label; neighbours; labels; degrees }

  (** [del_edge g n1 e n2] *)
  let del_edge g n1 e n2 =
    let i1 = id_of_node g n1 and i2 = id_of_node g n2 in
    let g = del_neighbour g i1 e i2 in
    del_neighbour g i2 e i1

  (** [edges g e] returns all the edges of kind [e] in [g]. *)
  let edges g e =
    let nbg = EdgeLabelMap.find e g.neighbours in
    let edges =
      NodeIdMap.fold (fun id ids edges ->
        NodeIdSet.fold (fun id' edges ->
          (NodeIdMap.find id g.labels, NodeIdMap.find id' g.labels) :: edges
        ) ids edges) nbg []
    in
    let edges = List.map (fun (n1, n2) ->
      if n1 < n2 then (n1, n2) else (n2, n1)
    ) edges
    in
    let edges = List.sort compare edges in
    ExtStd.List.uniq edges

  let min_degree g c nc =
    let cdegrees = EdgeLabelMap.find c g.degrees in
    let forbidden = EdgeLabelMap.find nc g.neighbours in
    let rec aux degrees =
      try
        let k, ids = IntMap.min_binding degrees in
        let rec aux' ids =
          try
            let id = NodeIdSet.choose ids in
            if NodeIdMap.find id forbidden = NodeIdSet.empty then
              Some (k, List.hd (NodeIdMap.find id g.labels))
            else
              aux' (NodeIdSet.remove id ids)
          with Not_found ->
            aux (IntMap.remove k degrees)
        in
        aux' ids
      with Not_found -> None
    in
    aux cdegrees

  (** [are_connected g n1 e n2] *)
  let are_connected g n1 e n2 =
    let id1 = id_of_node g n1 in
    let id2 = id_of_node g n2 in
    NodeIdSet.mem id2 (NodeIdMap.find id1 (EdgeLabelMap.find e g.neighbours))

  (** [pick_edge g e] *)
  let pick_edge g e =
    try
      let degrees = EdgeLabelMap.find e g.degrees in
      let k, ids = IntMap.max_binding degrees in
      if k = 0 then None
      else
        let id = NodeIdSet.choose ids in
        let nbg = EdgeLabelMap.find e g.neighbours in
        let nbgid = NodeIdMap.find id nbg in
        let id2 = NodeIdSet.choose nbgid in
        Some (List.hd (nodes_of_id g id), List.hd (nodes_of_id g id2))
    with Not_found ->
      None

  (** [merge g n1 n2] *)
  let merge g n1 n2 =
    let i1 = id_of_node g n1 and i2 = id_of_node g n2 in
    let nodes1 = nodes_of_id g i1 and nodes2 = nodes_of_id g i2 in
    let nbgs =
      List.map
        (fun e ->
           (e, List.filter (fun n -> not (List.mem n1 n) && not (List.mem n2 n))
              (neighbours g e n1 @ neighbours g e n2)))
        EdgeLabel.all
    in
    let g = del_node g n1 in
    let g = del_node g n2 in
    let g = add_node g (nodes1 @ nodes2) in
    List.fold_left (fun g (e, nbgs) ->
      List.fold_left (fun g ns ->
        add_edge g n1 e (List.hd ns)
      ) g nbgs
    ) g nbgs

  (** [all_labels g n] *)
  let all_labels g n =
    let i = id_of_node g n in
    nodes_of_id g i

  (** [show g labels] represents the graph [g] in the DOT format and
      uses [dotty] to display it. *)
  let show g labels =
    let dot_node (NodeId n, ns) =
      let ns =
        String.concat "," (List.map (fun n ->
          NodeLabel.to_string n
          ^ (match labels n with None -> "" | Some s -> " => " ^ s)
        ) ns)
      in
      Printf.sprintf "n%d [label=\"%s\"];" n ns
    in
    let dot_nodes =
      String.concat "\n" (List.map dot_node (NodeIdMap.bindings g.labels))
    in
    let seen = Hashtbl.create 13 in
    let dot_edge (NodeId n) c (NodeId n') =
      let n, n' = min n n', max n n' in
      if not (Hashtbl.mem seen (n, n')) then (
        Hashtbl.add seen (n, n') ();
        Printf.sprintf "n%d -- n%d [label=\"%s\"];" n n' (EdgeLabel.to_string c)
      ) else ""
    in
    let neighbour c (id, ids) =
      String.concat "\n" (List.map (dot_edge id c) (NodeIdSet.elements ids))
    in
    let dot_edges_of_kind (c, ngb) =
      String.concat "\n" (List.map (neighbour c) (NodeIdMap.bindings ngb))
    in
    let dot_edges =
      String.concat "\n"
        (List.map dot_edges_of_kind (EdgeLabelMap.bindings g.neighbours))
    in
    let dot =
      Printf.sprintf "graph g {\n%s\n%s\n}" dot_nodes dot_edges
    in
    let fname, cout = Filename.open_temp_file "flap" ".dot" in
    output_string cout dot;
    close_out cout;
    Printf.printf
      "Graph written in %s. (You need to install dotty to display it.)\n%!"
      fname;
    ignore (Sys.command ("dotty " ^ fname ^ "&"))

end
