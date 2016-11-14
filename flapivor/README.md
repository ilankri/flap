flapivor
========

# What is flapivor?

It is a continuous evaluation tool for flap projects. Flapivor is
watching periodically a list of projects described in an .mr file.  If
a project has been updated, it runs a bunch of tests on the project,
stores the results in the project repository, sends these results by
email to the authors and logs the evaluation in the global repository.

# How does it work?

## Initialization

The section describes what is done by `make init`.

First, each evaluation is supposed to be run inside a docker to limit
the risks implied by running untrusted code. The corresponding
Dockerfile is the Dockerfile at the root of this source tree.

Providing that two files named `id_rsa` and `id_rsa.pub` are given at
the root of this source tree, the initialization phase of the docker is
obtained by running:

```
docker build -t flapivor .
```

at the root of the source tree. This will build a docker image we
will use to evaluate each project.

Second, flapivor must be compiled:

```
ocamlbuild -use-ocamlfind flapivor
```

## flapivor

`flapivor` will periodically run 'mr' to check for new commits in
students projects. When a new commit is detected in a project P, a 
`flapitest` is run for P. `flapivor` makes sure that a project P
is tested by at most one `flapitest`.

## flapitest

`flapitest` clones the Docker image attached to the project repository
and to the test suite. Inside this docker image, it simply runs

```
make check
```

in the test suite.




