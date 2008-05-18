export OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = \
	main.ml

RESULT = ovt_gui

THREADS = yes
PACKS = oUnit
INCDIRS = \
	$(OCAMLLIBPATH)/cairo \
	$(OCAMLLIBPATH)/lablgtk2 \
	../../lib \
	../../lib/gtk 
LIBS = \
	$(OCAMLLIBPATH)/cairo/cairo \
	$(OCAMLLIBPATH)/cairo/cairo_lablgtk \
	../../lib/ovt \
	../../lib/gtk/ovtGtk
OCAMLBLDFLAGS = \
	$(OCAMLLIBPATH)/lablgtk2/lablgtk.cma \
	$(OCAMLLIBPATH)/lablgtk2/gtkThread.cmo
OCAMLNLDFLAGS = \
	$(OCAMLLIBPATH)/lablgtk2/lablgtk.cmxa \
	$(OCAMLLIBPATH)/lablgtk2/gtkThread.cmx

all:	clean native-code

check:	all

-include $(OCAMLMAKEFILE)
