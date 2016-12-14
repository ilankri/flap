# -*- makefile -*-
.PHONY: all publish clean

all: publish

publish:
	date > version.tex
	echo '\\begin{verbatim}' > changes.tex
	cat ChangeLog >> changes.tex
	echo '\\end{verbatim}' >> changes.tex
	pdflatex projet-2.tex
	mv projet-2.pdf compilation-m1-projet-2016-jalon-2.pdf

clean:
	rm -fr $(OUTDIR) version.tex changes.tex *.aux *.log *.out *.pdf
