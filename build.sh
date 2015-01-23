#!/bin/bash

#
# Author: Carlos Molina-Jimenez, 6 Jan 2015, Computer Lab. Univ. of Cambridge
#
# This build.sh uses only findlib compliant libraries. It compiles
# correctly yansh/MoanaML.
#

eval `opam config env`

#note: currently this only works with Irmin 0.8.3
ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package lwt,yojson,atdgen,irmin,irmin.backend,irmin.server,git.unix,irmin.unix -tag thread  tests.byte contacts.byte


./tests.byte
./contacts.byte
