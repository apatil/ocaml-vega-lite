.PHONY: all build install remove clean

all : build

deps=yojson

byte:
	ocamlbuild -use-ocamlfind -pkgs yojson vegaLite.cma

native:
	ocamlbuild -use-ocamlfind -pkgs yojson vegaLite.cmxa

build : byte native

install : all
	ocamlfind install vega-lite META _build/vegaLite.cm*

clean :
	- rm -rf _build
