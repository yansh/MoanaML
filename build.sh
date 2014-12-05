#!/bin/bash

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package yojson,atdgen,irmin,irmin.backend,irmin.unix -libs unix,lwt-unix,git-unix,irmin,irmin-server,irmin-unix -tag thread  tests.byte contacts.byte


./tests.byte
./contacts.byte
