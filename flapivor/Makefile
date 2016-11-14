.PHONY: all clean init eval

all:
	ocamlbuild -use-ocamlfind flapivor.byte

eval:
	mr up

init: all
	mr up
	docker build -t flapivor .

clean:
	ocamlbuild -clean

