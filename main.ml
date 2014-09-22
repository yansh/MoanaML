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
    List.filter (fun y -> print_endline ("< " ^(to_string y) ^ " > checking rule...<" ^ (to_string rule) ^ " > ");  Config.compare rule y) store;;
      db;;

  let to_list db = db;;

end;;

(* SQLlite based backend storage *)

module SQLStore:STORE = struct

   
  type t = Sqlite3.db
    
  let name = "SqlDB"
    
  let db = Msqlite.open_db name
     
  let add db tuple =  Msqlite.insert db tuple
          
  let query (db:t) (query: Config.tuple list) =   Msqlite.select db query

  let to_list db = [] (* TODO *)
end;;  

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

module G2:GRAPH = struct    
  
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
  

let t1 = {subj = Constant "a";
          pred = Constant "type";
          obj = Constant  "Car";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
            

let t2 = {subj = Constant "a";
          pred = Constant "hasColor";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
let t3 = {subj = Constant "b";
          pred = Constant "type";
          obj = Constant  "Chair";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
          
let t4 = {subj = Constant "b";
          pred = Constant "hasColor";
          obj = Constant  "Green";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;


let t5 = {subj = Constant "c";
          pred = Constant "type";
          obj = Constant  "Chair";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t6 = {subj = Constant "c";
          pred = Constant "hasColor";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
  
let tuples = [t1;t2;t3;t4;t5;t6];;

Config.print_tuples tuples;;

  (*let db = G.add t1 in
let db2 = G.add ~g:db t2 in 
    let db3 = G.add ~g:db2 t3 in
      let db4 = G.add ~g:db3 t4 in
     G.print db4;;*)  

(* Graph query *)
let q1 = {subj = Variable "?x";
          pred = Constant "type";
          obj =  Variable "?y";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let q2 = {subj = Variable "?x";
          pred = Constant "hasColor";
          obj =  Constant "Red";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let qry = [q1;q2];;
(*generate Moana graph with a functor Make and run a basic query template  *)

(*
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
(*
  let rec join l1 l2 = match l1, l2 with  
    | [] , [] -> []
    | _, [] -> []
    | [] , _ ->   []
    | h1::t1, h2::t2 -> 
      if h1.subj=h2.subj 
      then 
        [h1;h2] @  join t1 t2         
      else 
        join t1 l2  in let res=join l1 l2  in res;;
*)
  List.fold_right (fun tup1 acc ->
    let join = List.filter (fun tup2 ->
     tup1.subj = tup2.subj) l2 in
    if join = [] then acc else tup1 :: join @ acc) l1 [];;

print_endline "Running query, results ";;
print_tuples (qry2 tuples);(*;

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

(*Extended tuple: a tuple extended with a valuation of variables.*)
type ext_tuple = (string * Config.t Config.element_type) list * tuple list

let make_query (db : tuple list) (qry : tuple list) : tuple list list =
  (*Try to evaluate a variable occurring in query q, occurring
   * in the context of var_scope. If the variable doesn't appear
   * in the scope, then we simply return the variable.
   * Parameter f is a function applied to the retrieved value
   * before it is returned.
   * NOTE Annoyingly in OCaml there are no first-class selectors,
   *   so we must pass the selector as a parameter, s in this case.*)
  let try_eval s q var_scope f =
    match s q with
    | Variable x ->
        if List.mem_assoc x var_scope then
          f (List.assoc x var_scope)
        else
          q.subj
    | _ -> q.subj in
  (*Used to filter a part of a tuple record.
   * Returns a Boolean, indicating whether a match has occurred,
   * and possibly a variable-value pair (if we're matching against a variable)
   * -- this pair is used to extend the valuation which forms part of an
   * extended tuple.
   * s is a record selector
   * q is a query tuple
   * v is a tuple in the database*)
  let sub_filter s q v =
    match s q with
      (*Remaining variables are treated as wildcards,
       * but keep the value the variable evaluates to
       * since, if this tuple _does_ bring us closer to
       * a solution, we'll need to extent var_scope with
       * this mapping.*)
    | Variable x -> (true, Some (x, s v))
    | Constant _ -> (s q = s v, None)
    | Wildcard -> failwith "Malformed query" in
  let make_query' ((var_scope, pre_soln) : ext_tuple) (q : tuple) : ext_tuple list =
    (*q' is a specialised version of q wrt the current pre_soln*)
    let q' =
      { subj = try_eval (fun q -> q.subj) q var_scope (fun x -> x);
        pred = try_eval (fun q -> q.pred) q var_scope (fun x -> x);
        (*obj = try_eval (fun q -> q.obj) q var_scope (fun x -> t_to_objt x);
         * FIXME curious*)
        obj = try_eval (fun q -> q.obj) q var_scope (fun x -> x);
        ctxt = q.ctxt; (*FIXME ignored for the time being*)
        time_stp = q.time_stp; (*FIXME ignored for the time being*)
        sign = q.sign (*FIXME ignored for the time being*) } in
    List.fold_right (fun v acc ->
      let (m1, v1) = sub_filter (fun q -> q.subj) q' v in
      let (m2, v2) = sub_filter (fun q -> q.pred) q' v in
      let (m3, v3) = sub_filter (fun q -> q.obj) q' v in
      let scope_ext =
        List.fold_right (fun v acc ->
            match v with
            | None -> acc
            | Some x -> x :: acc) [v1; v2; v3] [] in
      (*FIXME remember that we're ignoring ctxt, time_stp and sign*)
      if m1 && m2 && m3 then
        (scope_ext @ var_scope, v :: pre_soln) :: acc
      else acc) db [] in
  List.fold_right (fun q acc ->
    List.map (fun ps -> make_query' ps q) acc
    |> List.concat) qry [([], [])]
  |> List.map snd;; (*Map ext_tuple to tuple, since we don't need the valuation any longer*)


print_endline "Running FANCY query, results ";;
print_tuples_list (make_query tuples qry);

