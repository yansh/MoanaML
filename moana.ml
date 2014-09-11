open Core.Std
  
module type STORE =
  sig
    type t
    
    (* storage name *)
    val name : string
      
    (*val init_storage: unit*)
    val add_fact : t -> unit
      
    (* provide a garph-query as list of tuples and returns list of tuples    *)
    (* matching it                                                           *)
    val query : 't list -> string list
      
  end;;
  
(* Signature for the Moana abstraction which will support many type of     *)
(* backend storage.                                                        *)
module type GRAPH =
  sig
          
    (*type tuple*)
      
    type storage
      
    val init: storage
    
    (*    val create_tuple : t -> t -> t -> t -> signature option  -> timestamp option -> tuple*)
          
    (* add fact as a tuple *)
    val add : storage-> Config.tuple -> storage
      
    (* specify a query as list of tuple, this will return a matching list of *)
    val map : storage -> Config.tuple list -> Config.tuple list       
      
  end;;
  
(* Functor from Element to MoanaGraph *)
(*module Make(S: STORE):GRAPH = struct 

  type t = S.t
    (*let storage = S.init_storage*)

    let add t =
        S.add_fact t
    
    let map list =
      S.query list;;

     
    let test msg = print_endline msg;;
    
     end ;;*)