open Printf
  
(* 
   List based storage
*)
module LStore:Moana.STORE = struct
  
  type t = Config.tuple list
    
  let name = "List store"
    
  let init = []
    
  let add storage tuple = storage @ [tuple] ;;

  let query (store:t) (query: Config.tuple list) = query;;

end;;

(* SQLlite based backend storage *)

module SQLStore:Moana.STORE = struct

   
  type t = Sqlite3.db
    
  let name = "Sql"
    
  let init = Msqlite.create_table name

     
  let add db tuple =  Msqlite.insert db tuple
          
  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query


end;;  

(* Moana GRAPH with List storage as a backend *)
module G:Moana.GRAPH = struct    
  
  module LS = LStore
    
    type t = LS.t
    
    let  init = LS.init
     
    let add t  tuple = 
      let x=print_endline "Adding fact" in 
          LS.add t tuple ;;
          
    let map (store:t) (query: Config.tuple list) = LS.query store query;;
      
end;;
(* Moana GRAPH with SQLite as backend storage *)

module G2:Moana.GRAPH = struct    
  
  module S = SQLStore
    
    type t = S.t
    
    let  init = S.init
     
    let add t  tuple = 
      let x=print_endline "Adding fact" in 
          S.add t tuple ;;
          
    let map (store:t) (query: Config.tuple list) = S.query store query;;
      
end;;
  

let t = Config.Tuple("subject", "predicate", "object", "context",None ,None);;


let db = G.init;;
G.add db t;;

let db = G2.init;;
G2.add db t;;

(*let sql_db = SQLStore.init;;
     SQLStore.add sql_db t;;*)

(* generate Moana graph with a functor Make *)
module MG = Moana.Make(LStore);;
let db = MG.init;;
MG.add db t;;
