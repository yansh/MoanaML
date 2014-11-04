#!/bin/bash
eval `opam config -env`
ocamlfind ocamlc -g -o moanaml -thread -linkpkg \
  -package core,yojson,lwt,lwt.unix,irmin,irmin.unix,sqlite3 \
  msqlite.ml config.ml moana.mli moana.ml moana_lists.ml moana_irmin.ml \
  tests.ml main.ml

./moanaml
