module MELLYSToDot : sig
  (** Take an arbitrary output function,a string prefix for node identifiers,
   * the net where the vertex comes from and a vertex. Generate
   * the declaration of a vertex with appriopriate graphical attributes
   * in dot format **)
  val generate_vert_dot : (string -> unit) -> string -> Net.MELLYS.t ->
    Net.MELLYS.vertex -> unit
  (** Take an arbitrary outout function, a string prefix for node identifiers
   * and an edge. Generate the declaration of an edge with appropriate graphical
   * attributes in dot format **)
  val generate_edge_dot : (string -> unit) -> string -> Net.MELLYS.edge -> unit
  (** Take an arbitrary outout function and a net. Generate an appropriate dot
   * header "digraph R {", use generate_vert_dot and generate_edge_dot
   * recursively on all vertices and edges of the graph and finally close the
   * digraph declaration, generating a full and correct dot description of the
   * net. **)
  val generate_net_dot : (string -> unit) -> Net.MELLYS.t -> unit
end
