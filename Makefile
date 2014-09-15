#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=lang.ml engine.ml test.ml main.ml 

TARGET=syncweb
PROGS=syncweb

ifeq ($(FEATURE_GUI),1)
PROGS+=xxx_browser
GUIEXECDIR=gui
endif

OPTPROGS= $(PROGS:=.opt)

#------------------------------------------------------------------------------
#package dependencies
#format: XXXDIR, XXXCMD, XXXCMDOPT, XXXINCLUDE (if different XXXDIR), XXXCMA

ifeq ($(FEATURE_XXX), 1)
XXXDIR=xxx
XXXCMD= $(MAKE) -C xxx &&  $(MAKE) xxx -C commons
XXXCMDOPT= $(MAKE) -C xxx &&  $(MAKE) xxx.opt -C commons
XXXCMA=xxx/xxx.cma  commons/commons_xxx.cma
XXXSYSCMA=xxx.cma
XXXINCLUDE=xxx
else
XXXCMD=
XXXCMDOPT=
endif

ifeq ($(FEATURE_PCRE), 1)
REGEXPDIR=ocamlpcre
REGEXPCMD= $(MAKE) -C ocamlpcre &&  $(MAKE) regexp -C commons
REGEXPCMDOPT= $(MAKE) -C ocamlpcre &&  $(MAKE) regexp.opt -C commons
REGEXPCMA=ocamlpcre/lib/pcre.cma  commons/commons_regexp.cma
#PCRESYSCMA=pcre.cma
REGEXPINCLUDE=ocamlpcre/lib
#PCREINCLUDE= +pcre
else
REGEXPCMD=
REGEXPCMDOPT=
endif



#------------------------------------------------------------------------------
SYSLIBS=str.cma unix.cma bigarray.cma $(XXXSYSCMA)
LIBS= commons/lib.cma \
      $(REGEXPCMA) \
      commons/commons_features.cma \
      globals/globals.cma

MAKESUBDIRS=commons \
  $(XXXDIR) \
  $(REGEXPDIR) \
  globals

INCLUDEDIRS=commons \
  $(XXXINCLUDE) \
  $(REGEXPINCLUDE) \
  globals

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all all.opt opt top clean distclean

#note: old: was before all: rec $(EXEC) ... but can not do that cos make -j20
#could try to compile $(EXEC) before rec. So here force sequentiality.

all:: Makefile.config
	$(MAKE) rec 
	$(MAKE) $(PROGS) 
opt:
	$(MAKE) rec.opt 
	$(MAKE) $(OPTPROGS) 
all.opt: opt
top: $(TARGET).top

rec:
	$(MAKE) -C commons 
	$(XXXCMD)
	$(BTCMD)
	$(REGEXPCMD)
	$(MAKE) features -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	$(MAKE) all.opt -C commons 
	$(XXXCMDOPT)
	$(BTCMDOPT)
	$(REGEXPCMDOPT)
	$(MAKE) features.opt -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 

#	+for D in $(MAKESUBDIRS); do $(MAKE) $$D || exit 1 ; done


syncweb: $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

syncweb.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^



$(TARGET).top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^




clean::
	rm -f syncweb syncweb.opt 
clean:: 
	rm -f $(TARGET).top
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config.ml
	rm -f TAGS
#	find -name ".#*1.*" | xargs rm -f

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)


##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

# note: don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	cp syncweb $(BINDIR)
	cp scripts/noweblatex $(BINDIR)
	@echo ""
	@echo "You can also install syncweb by copying the program"
	@echo "(available in this directory) anywhere you want and"

uninstall:
	rm -f $(BINDIR)/syncweb
	rm -f $(BINDIR)/scripts/noweblatex 


version:
	@echo $(VERSION)

##############################################################################
# Package rules
##############################################################################

PACKAGE=syncweb-$(VERSION)
TMP=/tmp

package: 
	make srctar 

srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz  --exclude=CVS --exclude=_darcs  $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)


# for the demo in mapreduce/, see the mapreduce/Makefile which produces
# the pdf, distribution_test, Makefile.test, etc

##############################################################################
# Website rules
##############################################################################

WEBSITE=/home/pad/mobile/homepage/software/project-syncweb

gen-html:
	emacs -l ~/.emacs --eval "(progn (htmlize-many-files '(\"changes.txt\")) (kill-emacs))"

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)
	cp readme.txt $(WEBSITE)
	cp changes.txt $(WEBSITE)
	make gen-html
	cp changes.txt.html $(WEBSITE)/changes-$(VERSION).html

##############################################################################
# Developer rules
##############################################################################

#VCS related

#test related

visual:
	~/pfff/codemap -no_legend -profile -ss 2 -filter pfff .
graph:
	~/pfff/codegraph -lang cmt -derived_data -build .

##############################################################################
# Pad specific rules
##############################################################################
