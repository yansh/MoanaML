 
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


(* SIGNATURES *)
  
   
module type STORE =
    sig
      type t 
        
      val empty : t
      
      (* storage name *)
      val name : string
        
        
      val init : ?query:Config.tuple list -> Config.tuple list -> t
      val add : t -> Config.tuple -> t
        
      (* provide a graph query as list of tuples and returns list of tuples    *)
      (* matching it                                                           *)
      val query :  t -> Config.tuple list -> Config.tuple list list
        
      (* return stored graph as set of tuples *)
      val to_list: t -> Config.tuple list
        
    end;;
  
(* Signature for the Moana abstraction which will support many types of    *)
(* backend storage.                                                        *)
module type GRAPH =
  sig
          
    (*type tuple*)
      
    type t 
		
    val init : ?query:Config.tuple list -> Config.tuple list -> t 
        
    (* add fact as a tuple *)
    val add : t -> Config.tuple -> t
      
    (* specify a query as list of tuple, this will return a matching list of *)
    val map : t -> Config.tuple list -> t (* Config.tuple list list*)
		
    val to_list: t -> Config.tuple list
                    
  end;;



(* Functor from STORE to GRAPH *)
module Make(S: STORE):(GRAPH with type t = S.t) = struct      
    
  type t = S.t
    
  let init = S.init 
           
  let add g (tuple : Config.tuple) =
     let s = Printf.sprintf "Adding fact to %s" S.name in
     print_endline s;
     S.add g tuple ;;
     
          
  let map g (query : Config.tuple list) = S.query g query |>  Helper.flatten_tuple_list |> S.init ~query
  
  let to_list = S.to_list 

  (*let to_string graph  =
    let dbList = S.to_list graph in
      let rec string_lst dbList =
          match dbList with
              | [] -> "Finished\n"
              | head :: rest ->
                Helper.to_string head ^ "\n" ^
                string_lst rest in
    string_lst dbList ;;*)
      
end ;;
