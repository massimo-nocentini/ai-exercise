
# Main Makefile, at this level it should be used to compile
# the TeX document, however it can be possibile to delegate
# prolog interaction to Makefile contained in `clp-fd` folder.

pdf:
	pdflatex doc.tex
