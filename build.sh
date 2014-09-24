#!/bin/bash

ocamlfind ocamlc -g -o moanaml -thread -linkpkg -package core,sqlite3 msqlite.ml config.ml tests.ml  moana.mli moana.ml main.ml
