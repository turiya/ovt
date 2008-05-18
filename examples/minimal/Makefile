export OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = \
	minimal.ml
RESULT = ovt_ex_minimal

INCDIRS = \
	$(OCAMLLIBPATH)/cairo \
	../../lib 
LIBS = \
	$(OCAMLLIBPATH)/cairo/cairo \
	../../lib/ovt

all:	clean native-code

check:	all

-include $(OCAMLMAKEFILE)
