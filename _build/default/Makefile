.PHONY: test check

build:
	dune build src

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	zip -r cs3110-finalproject.zip . -x _build/\* .git/\*

clean:
	dune clean
	rm -f cs3110-finalproject.zip

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

doc:
	dune build @doc