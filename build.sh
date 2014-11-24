#!/bin/bash

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package yojson,irmin,irmin.backend,irmin.unix -libs unix,lwt-unix,git-unix,irmin-server,irmin-unix,irmin -tag thread  tests.byte contacts.byte


./tests.byte
./contacts.byte
