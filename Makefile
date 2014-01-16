
Rfiles := mlaib2014/explore.R

slides.md: slides.Rmd $(Rfiles)
	R -e 'library(knitr); knit("$<")'
	sed 's/^% .*//' $@ > slides.md.tmp
	mv slides.md.tmp $@

slides.tex: slides.md slides-title.md preamble-slides.tex slides.template
	cat slides-title.md slides.md | \
	    pandoc -f markdown \
	    -t beamer \
	    --template slides.template \
	    -H preamble-slides.tex \
	    --latex-engine xelatex \
	    -V fontsize='16 pt' \
	    -o $@

# omitted: code for generated notes from the same Rmd source

pdfs := slides.pdf

$(pdfs): %.pdf: %.tex
	latexmk -xelatex $(basename $<)

all: $(pdfs)

.DEFAULT_GOAL := all

.PHONY: all
