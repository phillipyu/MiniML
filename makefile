all: Miniml Tests

Miniml: miniml.ml
	ocamlbuild miniml.byte

Tests: tests.ml
	ocamlbuild tests.byte

clean:
	rm -rf _build *.byte