#!/bin/bash

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package lwt,yojson,irmin,irmin.unix,oUnit -tag thread  tests.byte examples/contacts.byte


./tests.byte
./contacts.byte
