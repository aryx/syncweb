TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=indexer

SRC= index_pfff.ml \
     main_indexer.ml

#todo: use $(TOP)/external/ at some point for that too
PFFF=/home/pad/pfff
INCLUDE_LIBS=commons \
  h_program-lang graph_code \
  lang_cpp/parsing lang_c/parsing lang_c/analyze \
  lang_ml/parsing lang_ml/analyze \

LINK_LIBS=commons globals\
  commons_wrappers/graph \
  h_program-lang graph_code matcher \
  lang_cpp/parsing  lang_c/parsing lang_c/analyze \
  lang_ml/parsing lang_ml/analyze \

INCLUDEDIRS=$(INCLUDE_LIBS:%=$(PFFF)/%/) ../lpizer
SYSLIBS=nums.cma bigarray.cma unix.cma str.cma
LIBS= $(PFFF)/external/ocamlgraph/ocamlgraph.cma \
      $(PFFF)/commons/lib.cma \
      $(PFFF)/commons_core/commons_core.cma \
     $(LINK_LIBS:%=$(PFFF)/%/lib.cma) 

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET)
all.opt: $(TARGET).opt

$(TARGET): $(LIBS) ../lpizer/find_source.cmo $(OBJS) 
	$(OCAMLC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) -o $@ $^
