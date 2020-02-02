FROM ocaml/opam2

RUN sudo apt-get update && \
    sudo apt-get install -y gcc-mips-linux-gnu gdb-multiarch qemu-user \
			    jasmin-sable openjdk-11-jre m4
COPY --chown=opam . flap
WORKDIR flap
RUN opam switch 4.08 && eval $(opam env) && make dependencies byte-debug flap
