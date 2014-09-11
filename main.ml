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

(* generate Moana graph with a functor Make *)
module MG = Moana.Make(LStore);;
let db = MG.init;;
MG.add db t;;
