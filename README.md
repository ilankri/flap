# Initial projects

## Links

* http://moule.informatique.univ-paris-diderot.fr:8080/Yann/compilation-m1-2016

* http://moule.informatique.univ-paris-diderot.fr:8080/letouzey/compilation-m2

## Members

* Yung-Kun Hsieh
* Belaid Lagha
* Idir Lankri

# Overview of features

## Fully implemented

* Typechecker for Hopix
* Compiler from Hopix to Hobix
* Compiler from Hobix to Fopix
* Compiler from Fopix to Retrolix (without calling conventions)

## Partially implemented

* Compiler from Retrolix to MIPS assembly language
* Register allocation (only the liveness analysis is implemented)

## To be implemented

* Graph coloring of interference graph (to complete register allocation)
* Handling of strings and chars
* Type inference for Hopix
* Various optimizations like efficient compilation of pattern matching
  or tail call optimization

# How to test the generated code

Run `./docker-run.sh -mips foo.hopix` each time you want to compile and
execute `foo.hopix` to MIPS in a Docker container.
