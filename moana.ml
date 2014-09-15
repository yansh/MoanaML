open Core.Std
  
(* Signature for the STORE backend*)
 
module type STORE =
  sig
    type t 
      
    val db: t
    
    (* storage name *)
    val name : string
      
      
    (*val init_storage: unit*)
    val add : t -> Config.tuple -> t
      
    (* provide a garph-query as list of tuples and returns list of tuples    *)
    (* matching it                                                           *)
    val query :  t -> Config.tuple list -> Config.tuple list
      
  end
  
(* Signature for the Moana abstraction which will support many type of     *)
(* backend storage.                                                        *)
module type GRAPH =
  sig
          
    (*type tuple*)
      
    type t 
     
    val graph: t     
    (* add fact as a tuple *)
    val add : ?graph:t -> Config.tuple -> t
      
    (* specify a query as list of tuple, this will return a matching list of *)
    val map : ?graph:t -> Config.tuple list -> Config.tuple list       
      
  end
  
(* Functor from STORE to GRAPH *)
module Make(S: STORE):GRAPH = struct      
    
   type t = S.t
    
   let graph = S.db
            
   let add ?(graph=S.db) (tuple:Config.tuple) =       
     let s= sprintf "Adding fact to %s" S.name in        
      let x=print_endline s in  
       S.add graph tuple ;;
     
          
   let map ?(graph=S.db) (query: Config.tuple list) = S.query graph  query;;
      
end ;;