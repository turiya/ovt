export OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = unit.ml
RESULT = ovt_unit

PACKS = oUnit
INCDIRS = \
	$(OCAMLLIBPATH)/cairo \
	../../lib 
LIBS = \
	$(OCAMLLIBPATH)/cairo/cairo \
	../../lib/ovt

all:	clean native-code

check:	all
	./ovt_unit

-include $(OCAMLMAKEFILE)
