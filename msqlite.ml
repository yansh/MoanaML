open Printf

open Core.Std
exception Dummy

open Sqlite3
let create_table db =
	try
		let sql =
			sprintf "CREATE TABLE moana (s varchar(10), p varchar(10), o varchar(10))"
		in
		(printf "%s\n%!" sql;
			match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???")
			with
			| Rc.OK -> db
			| Rc.ERROR -> printf "Identified error: %s\n" (errmsg db); db
			| _ -> assert false)
	with | _ -> assert false

let open_db name =
	let db = Sqlite3.db_open name in
	ignore(create_table db);
	db

let insert db tuple =
	let sql = sprintf "INSERT INTO tbl VALUES ('subject', 'predicate', 'object')"
	in
	(printf " %s\n%!" sql;
		try
			match Sqlite3.exec db sql ~cb: (fun _ _ -> print_endline "???") with
			| Sqlite3.Rc.OK -> (printf "Inserted %d rows\n%!" (Sqlite3.changes db); db)
			| _ -> assert false
		with
		| xcp -> assert false)

let select db query =
	let sql = sprintf "SELECT * FROM tbl" in
	try
		(print_endline "TESTING!";
			match Sqlite3.exec db sql
				~cb: (fun _ _ -> (print_endline "FOUND!"; raise Dummy))
			with
			| Sqlite3.Rc.OK -> (print_endline "OK"; query)
			| _ -> assert false)
	with | xcp -> assert false
(* let () = createTable db;; insert db;; query db;; *)
