(* List based storage *)

open Moana

module S : STORE = struct

  type t = Config.tuple list

  let name = "List store"

  let db = []

  let add storage tuple = tuple :: storage

  let query (store : t) (q : Config.tuple list) = Config.execute_query store q

  let to_list db = db
end


(* Moana GRAPH with List storage as a backend *)
module G : GRAPH = struct

  module LS = S

  type t = LS.t

  let graph = LS.db

  let add ?(g = graph) (tuple : Config.tuple) =
    let s = Printf.sprintf "Adding fact to [ %s <- %s ]" LS.name (Config.to_string tuple) in
    print_endline s;
    LS.add g tuple

  let map ?(g = graph) (query : Config.tuple list) = LS.query g query

  let to_string graph  =
    let dbList = LS.to_list graph in
      let rec string_lst dbList =
          match dbList with
              | [] -> "Finished\n"
              | head :: rest ->
                Config.to_string head ^ "\n" ^
                string_lst rest in
    string_lst dbList
end
