export OCAMLMAKEFILE = ../../OCamlMakefile

PACKS = oUnit
INCDIRS = \
	$(OCAMLLIBPATH)/cairo \
	../../lib 
LIBS = \
	bigarray \
	$(OCAMLLIBPATH)/cairo/cairo \
	../../lib/ovt

SOURCES = \
	main.ml
RESULT = ovt_speed

all:	clean native-code

check:	all

-include $(OCAMLMAKEFILE)
