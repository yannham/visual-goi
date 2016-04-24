module type INET_TYPE = sig
  type vertex_type
  type label 
  val default : label
  val end_type : vertex_type
end

module type INET = sig
  exception InvalidNetException of string

  type port = AuxPort of int | MainPort
  type vertex_type

  (** Label of edges *)
  type label

  (** Include all features of a classical graph *)
  include Graph.Sig.G with type E.label = label

  (** The special vertex where end all the conclusions of a net *)
  val end_vertex : vertex

  (** The following functions are used to create,clear or copy a net *)
  val create : ?size:int -> unit -> t
  val clear : t -> unit
  val copy : t -> t

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

  (** Return the list of premises or conclusions of a vertex *)
  val premises : t -> vertex -> edge list
  val conclusion : t -> vertex -> edge
  val merge : t -> t -> unit 
end

module type REWRITABLE_INET = sig
  include INET

  val is_redex : E.t -> bool
  val reduce : t -> E.t -> unit
end
