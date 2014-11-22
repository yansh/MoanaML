#!/bin/bash

eval `opam config env`

<<<<<<< HEAD
ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package yojson,irmin,irmin.backend -libs unix,lwt-unix,git-unix,irmin-server,irmin-unix,irmin -tag thread  tests.byte contacts.byte
=======
ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package yojson,sqlite3,irmin,irmin.backend -libs unix,lwt-unix,git-unix,irmin-server,irmin-unix,irmin -tag thread  tests.byte contacts.byte
>>>>>>> eb6d48a389221bb0bf1a91b2a2ec03e46778db43

./tests.byte
./contacts.byte
