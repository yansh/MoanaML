(* Auto-generated from "contact.atd" *)


type contact = Contact_t.contact = {
  id: int;
  fn: string;
  last: string;
  email: string;
  work: string option;
  image: string option;
  twitterId: string option;
  title: string option;
  knows: string list option
}

val write_contact :
  Bi_outbuf.t -> contact -> unit
  (** Output a JSON value of type {!contact}. *)

val string_of_contact :
  ?len:int -> contact -> string
  (** Serialize a value of type {!contact}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contact :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contact
  (** Input JSON data of type {!contact}. *)

val contact_of_string :
  string -> contact
  (** Deserialize JSON data of type {!contact}. *)

