open Core.Std
  
(* Signature for the STORE backend*)
 
module type STORE =
  sig
    type t 
    
    (* storage name *)
    val name : string
      
    val init : t
      
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
     
     
    val init : t
    (* add fact as a tuple *)
    val add : t -> Config.tuple -> t
      
    (* specify a query as list of tuple, this will return a matching list of *)
    val map : t -> Config.tuple list -> Config.tuple list       
      
  end
  
(* Functor from STORE to GRAPH *)
module Make(S: STORE):GRAPH = struct      
    
    type t = S.t
    
    let  init = S.init
 (*  let create_tuple s p o c sg ts =
      let tuple =  Config.Tuple(s, p, o, c, sg, ts) in
         tuple;;*)
     
    let add t  (tuple:Config.tuple) = 
      let x=print_endline "Adding fact" in 
           S.add t tuple ;;
          
let map (store:t) (query: Config.tuple list) = S.query store query;;
      
end ;;