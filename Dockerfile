FROM ocaml/opam2

RUN sudo apt-get update && \
    sudo apt-get install -y gcc-mips-linux-gnu gdb-multiarch qemu-user \
			    jasmin-sable openjdk-11-jre m4
RUN opam update && opam install -y dune menhir pprint
COPY --chown=opam . flap
WORKDIR flap
RUN eval $(opam config env) && make byte-debug flap
