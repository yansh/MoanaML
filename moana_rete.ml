(*
* Copyright (c) 2015 Yan Shvartzshnaider
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


(* Rete based storage *)

open Moana

open Config

module S : STORE = struct

  type t = Rete.rete_dataflow

  let name = "Rete store"

 (* the notion of empty data flow doesn't exist *)
  let empty = Rete.Empty

let q1 =
  {
    subj = Variable "?x";
    pred = Constant "type";
    obj = Constant "Car";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }
  
let q2 =
  {
    subj = Variable "?x";
    pred = Constant "hasColor";
    obj = Constant "Red";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }
 
  let hlp = function 
		| None ->  []
		| Some x -> x


  let init ?query tuples = hlp query |> function
		| [] -> Rete.Empty
	  |	queries -> Rete.to_rete_dataflow queries tuples (*let n = Rete.to_rete_dataflow queries tuples in let Rete.Node (_, bm, _) = n 
		in let p2 = Rete.print_bm bm in n*)

  let add storage tuple =  Rete.add storage tuple

  (* execute a query on a store, given as collecton of tuples *)

  let query (store : t) (q : Config.tuple list) =  (*let Rete.Node (_, bm, _) = store in let p2 = Rete.print_bm bm in
	 let  p = print_string (string_of_int (List.length (Helper.flatten_tuple_list (Rete.get_sol_tuples store)))) in*) 
	store |>  Rete.get_sol_tuples 

  let to_list store = 
		(*let  p = print_string "--->";print_string (string_of_int (List.length (Helper.flatten_tuple_list (Rete.get_sol_tuples store)))) in*) 
		Rete.get_sol_tuples store |> Helper.flatten_tuple_list 
end


(* Moana GRAPH with List storage as a backend *)
module G : GRAPH = struct

  module RS = S

  type t = RS.t

  let graph = RS.empty
	
	let init = RS.init

  let add g (tuple : Config.tuple) =
  let s = Printf.sprintf "Adding fact to [ %s <- %s ]" RS.name (Helper.to_string tuple) in
    print_endline s;
    RS.add g tuple


  (* in RETE the result is located in the last BM in the network, no need to run any mechanism *)
	let map g (query : Config.tuple list) = g
	
	let to_list  = RS.to_list 
				

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
