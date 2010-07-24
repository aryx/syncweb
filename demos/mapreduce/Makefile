##############################################################################
# Variables
##############################################################################

SRC=distribution.ml distribution_test.ml

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

INCLUDES=-I commons -I ocamlmpi

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

distribution_test: distribution.mli distribution.ml distribution_test.ml
	$(OCAMLC) -c distribution.mli
	$(OCAMLC) -c distribution.ml
	$(OCAMLC) -c distribution_test.ml
	$(OCAMLC) -custom $(SYSLIBS) commons/commons.cma ocamlmpi/mpi.cma distribution.cmo distribution_test.cmo -o $@ 

clean::
	rm -f distribution_test


##############################################################################
# Literate programming
##############################################################################

SYNCFLAGS=-md5sum_in_auxfile -less_marks

SYNCWEB=../../syncweb $(SYNCFLAGS)
NOWEB=../../scripts/noweblatexpad
NOTANGLE=notangle

# for dot2texi package, need --shell-escape
PDFLATEX=pdflatex --shell-escape 


#------------------------------------------------------------------------------
# distribution example related
#------------------------------------------------------------------------------
TIMEFORMAT="TIME: %es"
EXPECTEDSIMPLE=expected_res_simple.out
EXPECTEDMPI=expected_res_mpi.out
BENCHRES=expected_time.out

all2: 
	make sync
	make distribution_test
	./distribution_test > $(EXPECTEDSIMPLE)
	mpirun -p4pg config.pg ./distribution_test > $(EXPECTEDMPI)  2>&1
	cat $(EXPECTEDMPI)
	make pdf
	make test

compile: 
	make sync
	make distribution_test

test:
	make -f Makefile.test
	make -f Makefile.test copy_binary
	echo "---- without MPI, naive_map_reduce ---" > $(BENCHRES)
	time -f $(TIMEFORMAT) -a -o $(BENCHRES) ./distribution_test 
	scp distribution_test phantom:/tmp/
	echo "---- with MPI  ---" >> $(BENCHRES)
	time -f $(TIMEFORMAT) -a -o $(BENCHRES)  mpirun -p4pg config.pg ./distribution_test 



#------------------------------------------------------------------------------
sync:
	$(SYNCWEB) distribution.ml.nw distribution.mli
	$(SYNCWEB) distribution.ml.nw distribution.ml
	$(SYNCWEB) distribution.ml.nw distribution_test.ml
#TOREPUT
#	$(SYNCWEB) -lang shell distribution.ml.nw Makefile.test
#	$(SYNCWEB) -lang shell distribution.ml.nw config.pg
#	$(SYNCWEB) -lang shell distribution.ml.nw get_dependencies.sh
#	chmod a+x get_dependencies.sh

#	$(NOTANGLE) distribution.ml.nw -RMakefile.test > Makefile.test
#	$(NOTANGLE) distribution.ml.nw -Rconfig.pg > config.pg
#	$(NOTANGLE) distribution.ml.nw -Rget_distribute_dependencies.sh > get_distribute_dependencies.sh

pdf:
	$(NOWEB) distribution.ml.nw > distribution.tex
	$(PDFLATEX) distribution.tex
	$(PDFLATEX) distribution.tex


lpclean::
	rm -f distribution.ml distribution.mli  
	rm -f distribution.tex
	rm -f distribution-dot2tex*
#	rm -f Makefile.test 
	rm -f config.pg get_dependencies.sh
	rm -f distribution_test.ml  
	rm -f distribution.pdf 
	rm -f distribution.out


#------------------------------------------------------------------------------
clean::
	rm -f *.aux *.dvi *.log *.toc


##############################################################################
# Package rules
##############################################################################

VERSION=0.1
PACKAGE=mapreduce-$(VERSION)
TMP=/tmp

package:
	make srctar

srctar:
	cp distribution.pdf save.pdf
	make clean
	make lpclean
	make sync
	mv save.pdf distribution.pdf
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz \
          --exclude=CVS --exclude=_darcs  \
          --exclude=ocamlmpi --exclude=commons --exclude=latex \
          $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)

#install: distribution-latest.tgz 


##############################################################################
# Website rules
##############################################################################
WEBSITE=/home/pad/mobile/homepage/ocaml

#en-html:
#	emacs -l ~/.emacs --eval "(progn (htmlize-many-files '(\"changes.txt\")) (kill-emacs))"

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)
	rm -f $(WEBSITE)/LATEST_MAPREDUCE* $(WEBSITE)/mapreduce-latest.tgz
	cd $(WEBSITE); touch LATEST_MAPREDUCE_IS_$(VERSION); ln -s $(PACKAGE).tgz mapreduce-latest.tgz
	cp distribution.pdf $(WEBSITE)/mapreduce.pdf

#	cp readme.txt $(WEBSITE)
#	cp changes.txt $(WEBSITE)

WEBSITE=/home/pad/mobile/homepage/software/project-syncweb/demo
websitedemo:
	mkdir -p $(WEBSITE)
	cp distribution.ml.nw $(WEBSITE)
	cp distribution.pdf $(WEBSITE)
	cp distribution.ml distribution.mli $(WEBSITE)

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
