.PHONY: all build install remove clean test byte native

all : build

deps=yojson

byte:
	ocamlbuild -use-ocamlfind -pkgs yojson vegaLite.cma

native:
	ocamlbuild -use-ocamlfind -pkgs yojson vegaLite.cmxa

build : byte native

install : build
	ocamlfind install vega-lite META _build/vegaLite.cm*

test:
	ocamlbuild -use-ocamlfind -package ppx_deriving_yojson,alcotest,yojson -I examples test/test.byte --

clean :
	- rm -rf _build
	- rm -rf gen/_build
	- rm gen/gen.byte
	- rm test/test.byte
