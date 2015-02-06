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


(* List based storage *)

open Moana

module S : STORE = struct

  type t = Config.tuple list

  let name = "List store"

  let empty = []

  let init ?(query:Config.tuple list=[])  tuples = tuples

  let add storage tuple = tuple :: storage

  let query (store : t) (q : Config.tuple list) = Config.execute_query store q

  let to_list db = db
end


(* Moana GRAPH with List storage as a backend *)
module G : GRAPH = struct

  module LS = S

  type t = LS.t

  let graph = LS.empty
	
	let init = LS.init

  let add g (tuple : Config.tuple) =
    let s = Printf.sprintf "Adding fact to [ %s <- %s ]" LS.name (Helper.to_string tuple) in
    print_endline s;
    LS.add g tuple

  let map g (query : Config.tuple list) = LS.query g query |>  Helper.flatten_tuple_list |> LS.init 
	
	let to_list g = LS.to_list g 
				

  (*let to_string graph  =
    let dbList = LS.to_list graph in
      let rec string_lst dbList =
          match dbList with
              | [] -> "Finished\n"
              | head :: rest ->
                Helper.to_string head ^ "\n" ^
                string_lst rest in
    string_lst dbList *)
end
