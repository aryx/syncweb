##############################################################################
# Variables
##############################################################################

SRC=demo.ml 

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

INCLUDES=-I ../commons -I ocamlmpi

SYSLIBS=str.cma unix.cma bigarray.cma

##############################################################################
# Generic ocaml variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS?=-g -dtypes # -w A

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS?=


OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES) -thread

# can also be set via 'make static'
STATIC= #-ccopt -static

# can also be unset via 'make purebytecode'
BYTECODE_STATIC=-custom

##############################################################################
# Top rules
##############################################################################

all: demo



demo: $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^
clean::
	rm -f demo


multi_main: multi_main.mli multi_main.ml
	$(OCAMLC) -o $@ $^
clean::
	rm -f multi_main

##############################################################################
# Literate programming
##############################################################################

SYNCWEB=../syncweb
NOWEB=../scripts/noweblatex
NOTANGLE=notangle

# for dot2texi package, need --shell-escape
PDFLATEX=pdflatex --shell-escape 

SYNCFLAGS=-md5sum_in_auxfile -less_marks

#------------------------------------------------------------------------------
# demo example related
#------------------------------------------------------------------------------
sync:
	$(SYNCWEB) $(SYNCFLAGS) demo.ml.nw demo.mli
	$(SYNCWEB) $(SYNCFLAGS) demo.ml.nw demo.ml

pdf:
	$(NOWEB) demo.ml.nw > demo.tex
	$(PDFLATEX) demo.tex
	$(PDFLATEX) demo.tex

lpclean::
	rm -f demo.ml demo.mli 
	rm -f demo.tex main.pdf
	rm -f .md5sum_*

clean::
	rm -f demo_main.tex demo_main.pdf

#------------------------------------------------------------------------------
# multi example related
#------------------------------------------------------------------------------
SRC3=multi.nw multi1.nw multi2.nw
sync3:
	$(SYNCWEB) $(SYNCFLAGS) $(SRC3) multi_main.ml
	$(SYNCWEB) $(SYNCFLAGS) $(SRC3) multi_main.mli

pdf3:
	$(NOWEB) multi.nw > multi.tex
	$(PDFLATEX) multi.tex
	$(PDFLATEX) multi.tex

#old: pp -P multi.nw > multi_total.nw
#noweave -n -index -backend scopehack *.nw
#	$(NOWEBMULTI) multi.nw multi1.nw multi2.nw > multi.tex


lpclean::
	rm -f multi_main.ml multi_main.mli
	rm -f multi.pdf multi.tex multi_total.nw

#------------------------------------------------------------------------------
clean::
	rm -f *.aux *.dvi *.log *.toc


##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend: 
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ *.exe gmon.out #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
