#!/bin/zsh

set -e

eval `opam config env`
ocamlbuild -use-ocamlfind -pkgs ppx_tools,ppx_let,ppx_tools.metaquot,yojson,str gen.byte --
