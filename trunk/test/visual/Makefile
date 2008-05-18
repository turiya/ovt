export OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = \
	visual.ml \
	suite.ml
RESULT = ovt_visual

INCDIRS = \
	$(OCAMLLIBPATH)/cairo \
	../../lib 
LIBS = \
	$(OCAMLLIBPATH)/cairo/cairo \
	../../lib/ovt

all:	clean native-code

check:	all
	./ovt_visual && python gallery.py

-include $(OCAMLMAKEFILE)
