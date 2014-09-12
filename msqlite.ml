open Printf

open Core.Std  
exception Dummy

let open_db = Sqlite3.db_open "t"

let create_table name= 
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
           with | _ -> assert false
      
let insert db tuple =
    let x=print_endline "Adding fact" in 
        let sql = sprintf "INSERT INTO tbl VALUES ('subject', 'predicate', 'object')" 
        in
          (printf " %s\n%!" sql;
           try
             match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???") with
               | Sqlite3.Rc.OK -> (printf "Inserted %d rows\n%!" (Sqlite3.changes db); db)
             | _ -> assert false
           with 
         | xcp -> assert false)
    
          
      (*let insert db =
        (for i = 0 to 3 do
       (let sql = sprintf "INSERT INTO tbl%d VALUES ('a', 3, 3.14)" i
        in
          (printf "%d %s\n%!" i sql;
           try
             match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???") with
               | Sqlite3.Rc.OK -> printf "Inserted %d rows\n%!" (Sqlite3.changes db)
             | _ -> assert false
           with | xcp -> print_endline (Printexc.to_string xcp)))
         done;)*)

let select db query= 
    let sql = sprintf "SELECT * FROM tbl" in       
         try
           (print_endline "TESTING!";
            match Sqlite3.exec db sql
                    ~cb: (fun _ _ -> (print_endline "FOUND!"; raise Dummy))
            with
              | Sqlite3.Rc.OK -> (print_endline "OK"; query)
              | _ -> assert false)
         with | xcp ->  assert false 
      (*let () =
    createTable db;;
    insert db;;
         query db;;*)