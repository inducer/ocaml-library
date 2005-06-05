.PHONY: all clean

OCAML_FLAGS=

OBJECTS=  utility.ml \
  commandline.ml \
  heap.ml \
  astar.ml \
  lookahead.ml \
  matrix.ml \
  homogeneous.ml \
  relation.ml \
  spline.ml \
  symbolic.ml \
  genmatrix.ml \
  genhomogeneous.ml \
  scalartypes.ml \
  minimization.ml \
  ringqueue.ml

all: utilities.cma utilities.cmxa


%.cmi : %.mli
	ocamlc -c $(OCAML_FLAGS) $<

%.cmo : %.ml
	ocamlc -c -g $(OCAML_FLAGS) $<

%.cmx : %.ml
	ocamlopt -c $(OCAML_FLAGS) $<

utilities.cma: $(OBJECTS:.ml=.cmo) 
	ocamlc -a $(OCAML_FLAGS) $(OBJECTS:.ml=.cmo) -o $@ 

utilities.cmxa: $(OBJECTS:.ml=.cmx) 
	ocamlopt -a $(OCAML_FLAGS) $(OBJECTS:.ml=.cmx) -o $@ 

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.cmxi *.cmxa *.cma 

.depend : *.ml *.mli
	ocamldep *.{ml,mli} > .depend

include .depend
