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

Each evaluation is supposed to be run inside a docker to limit the
risks implied by untrusted code. The corresponding Dockerfile is the
Dockerfile at the root of this source tree.

The initialization phase of the docker is obtained by running:

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
students projects. When a new commit C is detected in a project P, a 
`flapitest` is run for P and C.

## flapitest

`flapitest` clones the Docker image and attaches to the project
repository and to the test suite. Inside this docker image, it simply
runs:

```
make
```

in the source tree folder and, then

```
make check
```

in the test suite folder. The output log is extracted from the docker
and stored in a folder named 'log' in the GIT repository. It is also
sent by email to the authors.




