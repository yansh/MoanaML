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
  
  
(* 
   List based storage
*)
module LStore:Moana.STORE = struct
  
  type t = Config.tuple list
    
  let name = "List"
    
  let db = []     
    
 let add storage tuple = storage @ [tuple] ;;

 (* TODO: Make it work *)
 let query (store:t) (query: Config.tuple list) =  
  match query with
  | [] -> print_endline "Done processing"; db
  |  rule::rest_of_rules ->  
    List.filter (fun y -> rule=y) db;;
      db;;

  let to_list db = db;;

end;;

(* SQLlite based backend storage *)

module SQLStore:Moana.STORE = struct

   
  type t = Sqlite3.db
    
  let name = "SqlDB"
    
  let db = Msqlite.open_db name
     
  let add db tuple =  Msqlite.insert db tuple
          
  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query

  let to_list db = [] (* TODO *)
end;;  

(* Moana GRAPH with List storage as a backend *)

module G:Moana.GRAPH = struct    
  
  module LS = LStore
    
  type t = LS.t
           
  let graph = LS.db
    
  let add ?(g=graph) (tuple:Config.tuple) =       
    let s= sprintf "Adding fact to [ %s <- %s ]" LS.name (Config.to_string tuple) in        
      let x=print_endline s in  
        LS.add g tuple ;;
          
  let map ?(g=graph)  (query: Config.tuple list) = LS.query graph query;;
                 
let print graph  =
    let dbList = LS.to_list graph in
      let rec print_lst dbList = 
          match dbList with
              | [] -> print_endline "Finished"
              | head::rest -> print_endline (Config.to_string head); print_lst rest in print_lst dbList ;; 
            
end;;

(* Moana GRAPH with SQLite as backend storage *)

module G2:Moana.GRAPH = struct    
  
  module S = SQLStore
    
  type t = S.t   
    
  let graph = S.db          
   
  let add ?(g=graph) tuple = 
    let s= sprintf "Adding fact to [ %s <- %s ]" S.name (Config.to_string tuple) in        
     let x=print_endline s in  
       S.add graph tuple ;;
          
      let map ?(g=graph)  (query: Config.tuple list) = S.query graph query;;


  let print graph  =
    let dbList = S.to_list graph in
     let rec print_lst dbList = 
          match dbList with
              | [] -> print_endline "Finished"
              |  head::rest -> print_endline (Config.to_string head); print_lst rest in print_lst dbList ;; 
end;; 
  


let t1 = Config.Tuple("subject", "predicate", "object", "context", None ,None);;
let t2 = Config.Tuple("subject1", "predicate", "object", "context", None ,None);;
let t3 = Config.Tuple("subject2", "predicate", "object", "context", None ,None);;
let t4 = Config.Tuple("subject3", "predicate", "object", "context", None ,None);;


let q1 = [Config.Tuple("*", "predicate", "*", "context", None ,None);];;
  
let tuples = [
  Config.Tuple("subject", "predicate", "object", "context", None ,None);
  Config.Tuple("subject1", "predicate", "object", "context", None ,None);
  Config.Tuple("subject2", "predicate", "object", "context", None ,None);
  Config.Tuple("subject3", "predicate", "object", "context", None ,None)];;

Config.print_tuples tuples;;
     
let db = G.add t1 in
  let db2 = G.add ~g:db t2 in 
  let db3 = G.add ~g:db2 t3 in
  let db4 = G.add ~g:db3 t4 in
      G.print db4;; 

  
(*generate Moana graph with a functor Make *)

module MG = Moana.Make(LStore);;
let g = MG.graph;;
MG.add  t1;;


module MG2 = Moana.Make(SQLStore);;
let g = MG2.graph;;
     MG2.add t2;;