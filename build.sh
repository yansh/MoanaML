#!/bin/bash

ocamlfind ocamlc -g -o moanaml -thread -linkpkg -package core,sqlite3,yojson,lwt,lwt.unix,irmin,irmin.unix msqlite.ml \
  config.ml moana.mli moana.ml moana_lists.ml moana_irmin.ml tests.ml moana.ml main.ml

./moanaml
