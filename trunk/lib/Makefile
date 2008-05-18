export OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = \
	attribute.ml \
	util.ml \
	orientation.ml \
	color.ml \
	point.ml \
	interval.mli interval.ml \
	box.ml \
	bin.mli bin.ml \
	transformation.ml \
	placement.ml \
	style.ml \
	primitive.ml \
	sceneGraph.ml \
	legend.ml \
	dataSet.mli dataSet.ml \
	axis.ml \
	grid.ml \
	frame.mli frame.ml \
	render.ml

DOC_DIR = ../doc
LIB_PACK_NAME = ovt
OCAMLFLAGS = -for-pack Ovt
RESULT = ovt
INCDIRS = $(OCAMLLIBPATH)/cairo

all:	native-code-library

check:	all

-include $(OCAMLMAKEFILE)
