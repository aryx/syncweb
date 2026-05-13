##############################################################################
# Noweb illustration
##############################################################################
simple.pdf: simple.orig
	noweave -index -latex simple.orig > simple.tex
	pdflatex simple.tex


script.rc: script.nw
	../../bin/syncweb -less_marks -md5sum_in_auxfile -lang shell script.nw script.rc
