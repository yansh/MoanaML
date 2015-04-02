(* Auto-generated from "rete_node.atd" *)


type val_type = [ `Constant | `Variable ]

type value = { t: val_type; value_: string }

type tuple = {
  s: value;
  p: value;
  o: value;
  cxt: value option;
  time_smp: value option;
  sign: value option
}

type am_json = {
  ptrn: tuple;
  tpls: tuple list;
  vrs: (string * (string * tuple) list) list
}

type bm_json = { sols: (string * (string * tuple list)) list }

type memory = [ `AM of am_json | `BM of bm_json ]

type node_json = [
    `Empty
  | `Node of (memory * memory * node_json)
  | `BNode of tuple list
]
