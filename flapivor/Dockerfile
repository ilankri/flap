FROM ocaml/opam
MAINTAINER Yann Regis-Gianas

USER opam
RUN opam update
RUN opam install ocamlfind menhir
RUN eval `opam config env`






