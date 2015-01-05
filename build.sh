#!/bin/bash

# author: Carlos Molina Jimenez
# date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
#

eval `opam config env`

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package core,async,cohttp.async,lwt,yojson,atdgen,irmin.unix,irmin.backend,irmin.server,git.unix,irmin.unix,sexplib.syntax,comparelib.syntax,bin_prot.syntax -tag thread server_moanairmin.byte
