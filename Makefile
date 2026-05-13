PAPER_PDF := artifacts/causal-forest-ohie-paper.pdf

.PHONY: compile-pdf paper clean count-words

compile-pdf: paper

paper: clean
	mkdir -p artifacts
	cd paper && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
	cp paper/main.pdf $(PAPER_PDF)

clean:
	latexmk -C paper/main.tex
	rm -f paper/main.run.xml

count-words:
	pdftotext $(PAPER_PDF) - | wc -w
