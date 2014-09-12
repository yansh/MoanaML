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
  exception Dummy
    
  type t = Sqlite3.db
    
  let name = "Sql store"
    
  let init = 
    let db = Sqlite3.db_open name in       
      try
        let sql =
            sprintf "CREATE TABLE tbl (s varchar(10), p varchar(10), o varchar(10))"
        in
        (printf "%s\n%!" sql;
         match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???") 
         with
           | Sqlite3.Rc.OK -> db
           | _ -> assert false)    
      with 
        | xcp -> assert false
        
      
     
  let add db tuple =     
      let x=print_endline "Adding fact" in 
        let sql = sprintf "INSERT INTO tbl VALUES ('subject', 'predicate', 'object')" 
        in
          (printf " %s\n%!" sql;
           try
             match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???") with
               | Sqlite3.Rc.OK -> (printf "Inserted %d rows\n%!" (Sqlite3.changes db); db)
             | _ -> assert false
           with 
             | xcp -> (print_endline (Printexc.to_string xcp); db))
          
  let query (db:t) (query: Config.tuple list) =     
     let sql = sprintf "SELECT * FROM tbl" in       
         try
           (print_endline "TESTING!";
            match Sqlite3.exec db sql
                    ~cb: (fun _ _ -> (print_endline "FOUND!"; raise Dummy))
            with
              | Sqlite3.Rc.OK -> (print_endline "OK"; query)
            | _ -> assert false)
         with | xcp -> (print_endline (Printexc.to_string xcp); query)       
end;;  
module G:Moana.GRAPH = struct    
  
  module LS = LStore
    
    type t = LS.t
    
    let  init = LS.init
     
    let add t  tuple = 
      let x=print_endline "Adding fact" in 
          LS.add t tuple ;;
          
    let map (store:t) (query: Config.tuple list) = LS.query store query;;
      
end;;


let t = Config.Tuple("subject", "predicate", "object", "context",None ,None);;


let db = G.init;;
G.add db t;;

let sql_db = SQLStore.init;;
SQLStore.add sql_db t;;
(* generate Moana graph with a functor Make *)
module MG = Moana.Make(LStore);;
let db = MG.init;;
MG.add db t;;
