TOP=..
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= flag.ml config.ml

INCLUDEDIRS=$(TOP)/external/commons

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

config.ml:
	@echo "config.ml is missing. Have you run ./configure?"
	@exit 1

distclean::
	rm -f config.ml
