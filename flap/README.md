# Overview of features

## Fully implemented

* Typechecker for Hopix

* Compiler from Hopix to Hobix

* Compiler from Hobix to Fopix

* Compiler from Fopix to Retrolix (without calling conventions)

## Partially implemented

* Compiler from Retrolix to MIPS assembly language

* Register allocation (only the liveness analysis is implemented)

## To be fixed

Compilation of operations on blocks in Retrolix/MIPS: there is maybe
a problem with procedure call conventions because operations on blocks
are OK up to Retrolix but some programs that use operations on blocks
are successfully executed on MIPS architecture.

## To be implemented

* Graph coloring of interference graph (to complete register allocation)

* Handling of strings and chars

* Type inference for Hopix

* Various optimizations like efficient compilation of pattern matching
  or tail call optimization


# How to test the generated code

Install:
- qemu-system-mips
- sshpass


1. Run './download.sh' to get the Qemu images.

2. Run './start.sh' to start a VM.

3. Run './configure.sh' to install gcc and gdb on the VM.

4. Run './install.sh' each time you want to install runtime.c on the
   VM.

5. Run './run.sh foo.hopix' each time you want to compile and execute
  foo.hopix to MIPS on the VM.
