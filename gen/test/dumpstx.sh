#!/bin/bash

# Dumps the AST for the code in stx.ml.

echo
echo "(* Original syntax: *)"

cat stx.ml

echo
echo "(* AST: *)"

ocamlfind ocamlc -package ppx_tools.metaquot -dparsetree stx.ml
