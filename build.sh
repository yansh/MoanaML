#!/bin/bash

eval `opam config env`

rm *.byte

atdgen -j Rete/rete_node.atd                                                    
atdgen -t Rete/rete_node.atd 

ocamlbuild -r -Is Rete,examples -use-ocamlfind -use-menhir -classic-display -package lwt,yojson,irmin,irmin.unix,oUnit,atdgen -tag thread  tests.byte examples/contacts.byte -cflags ' -annot'


./tests.byte
./contacts.byte
