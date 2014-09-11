module G:Moana.GRAPH = struct    
      
  type storage = Config.tuple list
    
  let init = []
    
 (*  let create_tuple s p o c sg ts =
      let tuple =  Config.Tuple(s, p, o, c, sg, ts) in
         tuple;;*)

  let add storage tuple = let x=print_endline "Adding fact" in storage @ [tuple] ;;
          
let map (store:storage) (query: Config.tuple list) = query;;
      
end

(*module MG:Moana.GRAPH = Moana.Make(RDFStore);;*)
let t = Config.Tuple("subject", "predicate", "object", "context",None ,None);;
(*MG.print_tuple t;;*)


let db = G.init;;
G.add db t;;