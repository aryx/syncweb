TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lpizer

SRC= find_source.ml \
     lpize_pfff.ml \
     main_lpizer.ml

#todo: use $(TOP)/external/ at some point for that too
PFFF=/home/pad/pfff
INCLUDE_LIBS=commons \
  h_program-lang graph_code \
  lang_c/parsing lang_cpp/parsing \
  lang_ml/parsing lang_ml/analyze \

LINK_LIBS=commons commons_core commons_wrappers/graph globals\
  h_program-lang graph_code \
  lang_c/parsing lang_cpp/parsing \
  lang_ml/parsing lang_ml/analyze \

INCLUDEDIRS=$(INCLUDE_LIBS:%=$(PFFF)/%/) \
  $(shell ocamlfind query dyp) \
  $(shell ocamlfind query ocamlgraph) \

SYSLIBS=nums.cma bigarray.cma unix.cma str.cma

LIBS=\
  $(shell ocamlfind query dyp)/dyp.cma \
  $(shell ocamlfind query ocamlgraph)/graph.cma \
  $(LINK_LIBS:%=$(PFFF)/%/lib.cma) \

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET)
all.opt: $(TARGET).opt

$(TARGET): $(LIBS) $(OBJS) 
	$(OCAMLC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) -o $@ $^
