#!/bin/bash

ocamlfind ocamlc -o moanaml -thread -linkpkg -package core,sqlite3 msqlite.ml config.ml moana.mli moana.ml main.ml
