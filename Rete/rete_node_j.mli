(* Auto-generated from "rete_node.atd" *)


type val_type = Rete_node_t.val_type

type value = Rete_node_t.value = { t: val_type; value_: string }

type tuple = Rete_node_t.tuple = {
  s: value;
  p: value;
  o: value;
  cxt: value;
  time_smp: value option;
  sign: value option
}

type am_json = Rete_node_t.am_json = {
  ptrn: tuple;
  tpls: tuple list;
  vrs: (string * (string * tuple) list) list
}

type solutions = Rete_node_t.solutions = {
  sols: (string * (string * tuple list)) list
}

type bm_json = Rete_node_t.bm_json

type memory = Rete_node_t.memory

type node_json = Rete_node_t.node_json

val write_val_type :
  Bi_outbuf.t -> val_type -> unit
  (** Output a JSON value of type {!val_type}. *)

val string_of_val_type :
  ?len:int -> val_type -> string
  (** Serialize a value of type {!val_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_val_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> val_type
  (** Input JSON data of type {!val_type}. *)

val val_type_of_string :
  string -> val_type
  (** Deserialize JSON data of type {!val_type}. *)

val write_value :
  Bi_outbuf.t -> value -> unit
  (** Output a JSON value of type {!value}. *)

val string_of_value :
  ?len:int -> value -> string
  (** Serialize a value of type {!value}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> value
  (** Input JSON data of type {!value}. *)

val value_of_string :
  string -> value
  (** Deserialize JSON data of type {!value}. *)

val write_tuple :
  Bi_outbuf.t -> tuple -> unit
  (** Output a JSON value of type {!tuple}. *)

val string_of_tuple :
  ?len:int -> tuple -> string
  (** Serialize a value of type {!tuple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tuple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tuple
  (** Input JSON data of type {!tuple}. *)

val tuple_of_string :
  string -> tuple
  (** Deserialize JSON data of type {!tuple}. *)

val write_am_json :
  Bi_outbuf.t -> am_json -> unit
  (** Output a JSON value of type {!am_json}. *)

val string_of_am_json :
  ?len:int -> am_json -> string
  (** Serialize a value of type {!am_json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_am_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> am_json
  (** Input JSON data of type {!am_json}. *)

val am_json_of_string :
  string -> am_json
  (** Deserialize JSON data of type {!am_json}. *)

val write_solutions :
  Bi_outbuf.t -> solutions -> unit
  (** Output a JSON value of type {!solutions}. *)

val string_of_solutions :
  ?len:int -> solutions -> string
  (** Serialize a value of type {!solutions}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_solutions :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> solutions
  (** Input JSON data of type {!solutions}. *)

val solutions_of_string :
  string -> solutions
  (** Deserialize JSON data of type {!solutions}. *)

val write_bm_json :
  Bi_outbuf.t -> bm_json -> unit
  (** Output a JSON value of type {!bm_json}. *)

val string_of_bm_json :
  ?len:int -> bm_json -> string
  (** Serialize a value of type {!bm_json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bm_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bm_json
  (** Input JSON data of type {!bm_json}. *)

val bm_json_of_string :
  string -> bm_json
  (** Deserialize JSON data of type {!bm_json}. *)

val write_memory :
  Bi_outbuf.t -> memory -> unit
  (** Output a JSON value of type {!memory}. *)

val string_of_memory :
  ?len:int -> memory -> string
  (** Serialize a value of type {!memory}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_memory :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> memory
  (** Input JSON data of type {!memory}. *)

val memory_of_string :
  string -> memory
  (** Deserialize JSON data of type {!memory}. *)

val write_node_json :
  Bi_outbuf.t -> node_json -> unit
  (** Output a JSON value of type {!node_json}. *)

val string_of_node_json :
  ?len:int -> node_json -> string
  (** Serialize a value of type {!node_json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_node_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> node_json
  (** Input JSON data of type {!node_json}. *)

val node_json_of_string :
  string -> node_json
  (** Deserialize JSON data of type {!node_json}. *)

