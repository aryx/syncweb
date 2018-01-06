TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= web_to_code.ml \
     crossref_chunk.ml crossref_code.ml web_to_tex.ml

INCLUDEDIRS=$(TOP)/external/commons $(TOP)/frontend

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)
