TOP=..
##############################################################################
# Variables
##############################################################################
-include $(TOP)/Makefile.config

TARGET=lib

SRC= common.ml common2.ml

INCLUDEDIRS= 

#nomore: graphics.cma, create conflict when link with lablgtk
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma 

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(PROGS)
all.opt:: $(TARGET).cmxa  $(PROGS:=.opt)

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)
