PAPER_PDF := artifacts/causal-forest-ohie-paper.pdf

.PHONY: compile-pdf paper clean copy-figs compile-notes count-words

compile-pdf: paper

paper: clean copy-figs
	mkdir -p artifacts
	cd compilation && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
	cp compilation/main.pdf $(PAPER_PDF)

clean:
	scripts/clean-tex.sh

copy-figs:
	mkdir -p content/figs
	cp figs/* content/figs/

compile-notes:
	scripts/notes-to-pdf.

count-words:
	pdftotext $(PAPER_PDF) - | wc -w
