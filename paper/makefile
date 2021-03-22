
INK=inkscape
PLOT=gnuplot

NAME=thesis

FIGURES=$(wildcard figures/*.svg)
FIGURES_PDF=$(patsubst %.svg,%.pdf,${FIGURES})

GRAPHS=$(wildcard graphs/*.plt)
GRAPHS_PDF=$(patsubst %.plt,%.pdf,${GRAPHS})


.PHONY: report clean spell

report: tables/knotinfo-rank.tex  data/template-example.txt ${FIGURES_PDF} ${GRAPHS_PDF}
	pdflatex ${NAME}.tex
	bibtex ${NAME}.aux
	pdflatex ${NAME}.tex
	pdflatex ${NAME}.tex

clean:
	rm -rf *.pdf *.bbl *.aux *.log
	rm tables/quasipositive.tex
	rm data/knotinfo.lisp
	rm figures/*.pdf
	rm figures/*.pdf_tex

spell:
	aspell --personal ${PWD}/spell_words.txt check ${NAME}.tex

tables/knotinfo-rank.tex: data/knotinfo.lisp tables/make-knotinfo-rank.lisp 
	tables/make-knotinfo-rank.lisp $@

data/knotinfo.lisp: data/knotinfo.csv data/convert-braid-csv.lisp
	data/convert-braid-csv.lisp $< $@

data/template-example.txt: data/make-template-example.lisp
	data/make-template-example.lisp > $@

figures/%.pdf: figures/%.svg
	${INK} -D -g --file=$< --export-pdf=$@ --export-latex

graphs/%.pdf: graphs/%.plt
	${PLOT} -e "set terminal pdf" -e "set output '$@'" $<






