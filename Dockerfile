FROM ocaml/opam

COPY --chown=opam . flap
WORKDIR flap
RUN opam update
RUN opam install ocamlfind menhir
RUN eval $(opam config env)
