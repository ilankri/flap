FROM ocaml/opam

RUN sudo apt-get update && \
    sudo apt-get install -y gcc-mips-linux-gnu qemu-user jasmin-sable \
			    openjdk-8-jre
RUN opam update && opam install -y ocamlfind menhir pprint
COPY --chown=opam . flap
WORKDIR flap
RUN eval $(opam config env) && make byte-debug flap
