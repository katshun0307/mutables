builder = dune

install: 
	opam install .

mutables: mutables.ml
	dune build mutables.cmxa

test: test.ml
	dune runtest
