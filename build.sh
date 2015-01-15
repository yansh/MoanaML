#!/bin/bash

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package lwt,yojson,atdgen,irmin,irmin.backend,irmin.server,git.unix,irmin.unix -tag thread  tests.byte contacts.byte


./tests.byte
./contacts.byte

#
# This version of build.sh uses findlib compliant libraries 
#
