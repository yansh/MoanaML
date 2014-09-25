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
open Tests
open OUnit2
    
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
let  query (store:t) (q: Config.tuple list) =  
  match q with
  | [] -> print_endline "Done processing"; db
  |  rule::rest_of_rules -> 
    List.filter (fun y -> print_endline ("< " ^(to_string y) ^ " > checking rule...<" ^ (to_string rule) ^ " > ");  
      Config.compare rule y) store;;
      db;;

  let to_list db = db;;

end;;

(* SQLlite based backend storage *)

  (*module SQLStore:STORE = struct

   
  type t = Sqlite3.db
    
  let name = "SqlDB"
    
  let db = Msqlite.open_db name
     
  let add db tuple =  Msqlite.insert db tuple
          
  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query

  let to_list db = [] (* TODO *)
     end;;*)  

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

(*module G2:GRAPH = struct    
  
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
*)


(* HARDCODED
   
 MAP ?x {    
     ?x type ?y
     ?x color, Red   
    }
     
*)
let qry2 l =
  let l1 = List.filter (fun tup ->
    tup.pred = Constant "type") l in (* all elements of predicate type *)
  let l2 = List.filter (fun tup ->
    tup.pred = Constant "hasColor" && tup.obj = Constant "Red") l in (* all elements that have color Red *)
  List.fold_right (fun tup1 acc ->
    let join = List.filter (fun tup2 ->
     tup1.subj = tup2.subj) l2 in
    if join = [] then acc else tup1 :: join @ acc) l1 [];;


 

(*============================== EXTRA ================================== *)
(*print_endline "Running query, results ";;
 print_tuples (qry2 tuples);*)  
    
(*generate Moana graph with a functor Make and run a basic query template  *)

(*;

module MG = Make(LStore);;
let g = MG.graph;;
let db = MG.add t1 in
let db2 = MG.add ~g:db t2 in 
    let db3 = MG.add ~g:db2 t3 in
      let db4 = MG.add ~g:db3 t4 in
          let tuples = MG.map  ~g:db4 qry in
            Config.print_tuples tuples;;

module MG2 = Make(SQLStore);;
let g = MG2.graph;;
     MG2.add t2;;
  
 *)

  (*let db = G.add t1 in
let db2 = G.add ~g:db t2 in 
    let db3 = G.add ~g:db2 t3 in
      let db4 = G.add ~g:db3 t4 in
     G.print db4;;*)  

