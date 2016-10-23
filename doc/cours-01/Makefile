SHELL = /bin/sh
RM = rm -f
OCAMLBUILD = ocamlbuild
DEBUGFLAG = -g
OCAMLCFLAGS = -cflags $(DEBUGFLAG),-annot,-bin-annot,-w,+a-50
OCAMLLFLAGS = -lflags $(DEBUGFLAG)
OCAMLLIBS = -libs unix

TARGET = marthe.native

build = $(OCAMLBUILD) $(OCAMLCFLAGS) $(OCAMLLFLAGS) $(OCAMLLIBS) $@
clean = $(OCAMLBUILD) -clean

.SUFFIXES:
.PHONY: all $(TARGET) clean

all: $(TARGET)

$(TARGET):
	$(build)

clean:
	$(clean)
