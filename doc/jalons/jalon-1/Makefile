# -*- makefile -*-
.PHONY: all publish clean

all: publish

publish:
	date > version.tex
	echo '\\begin{verbatim}' > changes.tex
	cat ChangeLog >> changes.tex
	echo '\\end{verbatim}' >> changes.tex
	pdflatex projet-1.tex
	mv projet-1.pdf compilation-m1-projet-2015-jalon-1.pdf

clean:
	rm -fr version.tex changes.tex *.aux *.log *.out *.pdf
