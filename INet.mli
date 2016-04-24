module Make(T : Sig.INET_TYPE) : Sig.INET with type vertex_type = T.vertex_type and type label = {src_port : port; dst_port : port; l : T.label}
