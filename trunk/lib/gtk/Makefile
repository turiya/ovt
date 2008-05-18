export OCAMLMAKEFILE = ../../OCamlMakefile

DOC_DIR = ../doc
SOURCES = \
	gui.mli gui.ml
LIB_PACK_NAME = ovtGtk
OCAMLFLAGS = -for-pack OvtGtk
THREADS = yes
RESULT = ovtGtk
INCDIRS = $(OCAMLLIBPATH)/cairo $(OCAMLLIBPATH)/lablgtk2 ..

all:	clean native-code-library

check:	all

-include $(OCAMLMAKEFILE)
