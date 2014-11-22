#!/bin/bash

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package yojson,sqlite3,irmin,irmin.backend -libs unix,lwt-unix,git-unix,irmin-server,irmin-unix,irmin -tag thread  tests.byte contacts.byte

./tests.byte
./contacts.byte
