(*
* Copyright (c) 2014 Yan Shvartzshnaider
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf
open Moana
open Config
(*
   List based storage
*)

(*============= IMPLEMENTATION ================== *)

module LStore:STORE = struct
  type t = Config.tuple list

  let name = "List"

  let db = []

  let add storage tuple = storage @ [tuple] ;;

  (* TODO: Make it work *)
  let  query (store : t) (q: Config.tuple list) =
    match q with
    | [] -> print_endline "Done processing"; db
    | rule :: rest_of_rules ->
      List.filter (fun y ->
       print_endline ("< " ^
        Config.to_string y ^
        " > checking rule...<" ^
        Config.to_string rule ^ " > ");
        Config.compare rule y) store;;
        db;;

  let to_list db = db;;
end;;

(* SQLlite based backend storage *)

module SQLStore:STORE = struct
  type t = Sqlite3.db

  let name = "SqlDB"

  let db = Msqlite.open_db name

  let add db tuple =  Msqlite.insert db tuple

  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query

  let to_list db = [] (* TODO *)
end;;

(* Moana GRAPH with List storage as a backend *)

module G:GRAPH = struct

  module LS = LStore

  type t = LS.t

  let graph = LS.db

  let add ?(g=graph) (tuple:Config.tuple) =
    let s= sprintf "Adding fact to [ %s <- %s ]" LS.name (Config.to_string tuple) in
    print_endline s;
    LS.add g tuple ;;

  let map ?(g=graph)  (query: Config.tuple list) = LS.query g query;;

let print graph  =
    let dbList = LS.to_list graph in
      let rec print_lst dbList =
          match dbList with
              | [] -> print_endline "Finished"
              | head::rest -> print_endline (Config.to_string head); print_lst rest in print_lst dbList ;;

end;;

(* Moana GRAPH with SQLite as backend storage *)

module G2:GRAPH = struct

  module S = SQLStore

  type t = S.t

  let graph = S.db

  let add ?(g=graph) tuple =
    let s= sprintf "Adding fact to [ %s <- %s ]" S.name (Config.to_string tuple) in
    print_endline s;
    S.add g tuple ;;

      let map ?(g=graph)  (query: Config.tuple list) = S.query g query;;


  let print graph  =
    let dbList = S.to_list graph in
     let rec print_lst dbList =
          match dbList with
              | [] -> print_endline "Finished"
              |  head::rest -> print_endline (Config.to_string head); print_lst rest in print_lst dbList ;;
end;;


let t1 = Tuple(Subject (Constant "a"), Predicate (Constant "type"), Object (Constant "Car"), Context (Constant "context"), None, None);;

let subj_ct x = Subject (Constant x)
let pred_ct x = Subject (Constant x)

let t1 = Tuple(subj_ct "a", Predicate (Constant "type"),  Object (Constant "Car"), Context( Constant "context"), None ,None);;
let t2 = Tuple(subj_ct "a", Predicate (Constant "hasColor"),  Object (Constant "Red"), Context(Constant "context"), None ,None);;
let t3 = Tuple(subj_ct "b", Predicate (Constant "type"),  Object (Constant "Chair"), Context(Constant "context"), None ,None);;
let t4 = Tuple(subj_ct "b", Predicate (Constant "hasColor"),  Object (Constant "green"), Context(Constant  "context"), None ,None);;

let tuples = [
Tuple(Subject (Constant "a"), Predicate (Constant "type"),  Object (Constant "Car"), Context( Constant "context"), None ,None);
Tuple(Subject (Constant "a"), Predicate (Constant "hasColor"),  Object (Constant "Red"), Context(Constant "context"), None ,None);
Tuple(Subject (Constant "b"), Predicate (Constant "type"),  Object (Constant "Chair"), Context(Constant "context"), None ,None);
Tuple(Subject (Constant "b"), Predicate (Constant "hasColor"),  Object (Constant "green"), Context(Constant  "context"), None ,None)];;

Config.print_tuples tuples;;

let db = G.add t1 in
let db2 = G.add ~g:db t2 in
    let db3 = G.add ~g:db2 t3 in
      let db4 = G.add ~g:db3 t4 in
          G.print db4;;

(* Graph query *)
let q1 = [Tuple(Subject (Variable "?x"), Predicate (Constant "type"),  Object (Variable "?y"), Context( Constant "context"), None ,None)] ;;

(*generate Moana graph with a functor Make and run a basic query template  *)
module MG = Make(LStore);;
let g = MG.graph;;
let db = MG.add t1 in
let db2 = MG.add ~g:db t2 in
    let db3 = MG.add ~g:db2 t3 in
      let db4 = MG.add ~g:db3 t4 in
          let tuples = MG.map  ~g:db4 q1 in
            Config.print_tuples tuples;;

module MG2 = Make(SQLStore);;
let g = MG2.graph;;
     MG2.add t2;
