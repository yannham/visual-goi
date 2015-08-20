(* t module is encapsulating a Graph (Graph.G) module type
 * by offering functions to manipulate a proof t.
 * type t,nvertex and nedge encapsulate types t,edge and vertex
 * "inherited" from Graph.G. However, functions like nedge_to_edge
 * nvert_to_vertex and t_to_graph allow to convert these types back
 * and to use any of the functionnality of the underlying OcamlGraph
 * implementation.
 *)

module MELLYS : sig
  type port = LeftPort | RightPort | MainPort | AuxPort of int | EndPort
  type vertex_type =
    | End
    | One
    | Bot
    | Axiom
    | Cut
    | Par
    | Tensor
    | Contraction
    | Weakening
    | Dereliction
    | Sync
    | BangBox
    | BotBox
    | YBox

  (** Label of edges *)
  type label = {src_port : port; dst_port : port; formula : Logic.MELL.formula}

  (** Include all features of a classical graph *)
  include Graph.Sig.G with type E.label = label

  (** The special vertex where end all the conclusions of a net *)
  val end_vertex : vertex

  (** The following functions are used to create,clear or copy a net *)
  val create : ?size:int -> unit -> t
  val clear : t -> unit
  val copy : t -> t

  (** This function does the same as List.find, iterating on edges *)
  val find_edge_pred : (edge -> bool) -> t -> edge

  (** All vertices have a unique id, and must be created using make_vertex *)
  val make_vertex : vertex_type -> vertex
  (** Get the kind of a vertex *)
  val vertex_get_type : vertex -> vertex_type

  val add_vertex : t -> vertex -> unit
  (** Create a new vertex, add it to the net, and return it *)
  val madd_vertex : t -> vertex_type -> vertex
  val remove_vertex : t -> vertex -> unit

  val add_edge_e : t -> edge -> unit
  val remove_edge_e : t -> edge -> unit
  (** Bang (and other) boxes are handled through the three following functions.
   * A box in a net is just seen as a simple vertex. The net inside the box must
   * be set, unset and obtain through these functions, that take the net containing
   * the box and the vertex corresponding to the box *)
  val bind_box_content : t -> vertex -> t -> unit
  val unbind_box_content : t -> vertex -> unit
  val box_content : t -> vertex -> t

  (** Take a net, a new vertex type to add, a list of its premises
   * as couples of edges and port and a list of labels for its conclusions. 
   * Add the vertex to the net, set the (already existing) premises to end
  * on this vertex (branched on the port specified in the couple)
  * and create then add new edges as conclusions, using given labels. *)
  val add_vertex_premises_conclusions :
    t -> vertex_type -> (edge * port) list ->
    E.label list -> vertex * edge list

  (** Return the list of premises or conclusions of a vertex *)
  val premises : t -> vertex -> edge list
  val conclusions : t -> vertex -> edge list
  (** Return the list of pending conclusions of a net
   * (equivalent to premises t end_vertex) *)
  val net_conclusions : t -> edge list

  (** Create a new net containing an axiom and its conclusions *)
  val axiom : Logic.MELL.formula -> t * vertex * edge * edge
  (** Create a new net containing a one and its conclusion *)
  val one : unit -> t * vertex
  (** Create a new net containing a bot and its conclusion *)
  val bot : unit -> t * vertex

  (** The following functions implements inductive rules to build up
   * net from axioms, ones and bots. Edges are active formulas.
   * Rules which require two net as premises (mix, tensor and cut) store
   * the result in the first net and clear the second one.
   * Rules which implements boxes (bang, ...) create a new net,
   * containing just the box vertex and its conclusions, and bind
   * the box to its content. 
   * Return the newly created vertex and its conclusion,
   * with the created net for box rules *)
  val mix : t -> t -> unit
  val tensor : t -> edge -> t -> edge -> vertex * edge
  val par : t -> edge -> edge -> vertex * edge
  val cut : t -> edge -> t -> edge -> vertex
  val der : t -> edge -> vertex * edge
  val weak : t -> Logic.MELL.formula -> vertex * edge 
  (** The bang function provides a list that associates
   * all former ?A conclusions to their new counterpart *)
  val bang : t -> edge -> t * vertex * edge * (edge*edge) list
  val cntr : t -> edge -> edge -> vertex * edge
  val sync : t -> edge list -> vertex * edge list
end
