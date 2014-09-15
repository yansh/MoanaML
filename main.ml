open Printf
  
(* 
   List based storage
*)
module LStore:Moana.STORE = struct
  
  type t = Config.tuple list
    
  let name = "List"
    
  let db = []     
    
  let add storage tuple = storage @ [tuple] ;;

  let query (store:t) (query: Config.tuple list) = query;;

end;;

(* SQLlite based backend storage *)

module SQLStore:Moana.STORE = struct

   
  type t = Sqlite3.db
    
  let name = "SqlDB"
    
  let db = Msqlite.open_db name
     
  let add db tuple =  Msqlite.insert db tuple
          
  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query


end;;  

(* Moana GRAPH with List storage as a backend *)
module G:Moana.GRAPH = struct    
  
  module LS = LStore
    
    type t = LS.t
           
    let graph = LS.db
       
    let add ?graph:t tuple = 
      let s= sprintf "Adding fact to %s" LS.name in        
      let x=print_endline s in  
          LS.add graph tuple ;;
          
    let map ?graph:t (query: Config.tuple list) = LS.query graph query;;
      
end;;
(* Moana GRAPH with SQLite as backend storage *)

module G2:Moana.GRAPH = struct    
  
  module S = SQLStore
    
    type t = S.t   
    
    let graph = S.db          
     
    let add ?graph:t  tuple = 
      let s= sprintf "Adding fact to %s" S.name in        
      let x=print_endline s in  
      S.add graph tuple ;;
          
    let map ?graph:t (query: Config.tuple list) = S.query graph query;;
      
end;; 
  

let t = Config.Tuple("subject", "predicate", "object", "context",None ,None);;


G.add t;;

G2.add t;;

(*let sql_db = SQLStore.init;;
     SQLStore.add sql_db t;;*)

(*generate Moana graph with a functor Make *)

module MG = Moana.Make(LStore);;
let g = MG.graph;;
MG.add  t;;


module MG2 = Moana.Make(SQLStore);;
let g = MG2.graph;;
MG2.add t;;

