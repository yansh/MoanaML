#!/bin/bash

eval `opam config env`

ocamlbuild -use-ocamlfind -use-menhir -classic-display -package yojson,sqlite3,irmin,irmin.backend -libs unix,lwt-unix,git-unix,irmin-server,irmin-unix,irmin,sqlite3 -tag thread  tests.byte

./tests.byte

