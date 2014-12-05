#!/bin/bash

eval `opam config env`

ocamlbuild -use-ocamlfind -use-menhir  -classic-display -classic-display -package yojson,atdgen -package js_of_ocaml -package js_of_ocaml.syntax  -syntax camlp4o jsContacts.byte
js_of_ocaml jsContacts.byte
google-chrome --allow-file-access-from-files index.html 

