all:
	dune build

test:
	dune runtest

clean:
	dune clean

run:
	dune exec review

.PHONY: all test clean run
