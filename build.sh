#!/bin/bash

ocamlfind ocamlc -g -o moanaml -thread -linkpkg -package core,sqlite3 msqlite.ml \
  config.ml moana.mli moana.ml moana_lists.ml tests.ml main.ml

