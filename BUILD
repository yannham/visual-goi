To build VisualGoI, you need an recent OCaml installation with additionnal packages :
 - unix
 - menhir
 - ocamlgraph
(and, later, js_of_caml)

You can then run in the main directory
ocamlbuild -use-menhir -pkgs unix,ocamlgraph main.native
(or main.byte for OCaml byte code, or any other target)

You may need the "dot" tool from graphviz, that VisualGoI
use to convert its dot representation of proof nets to
svg images.
