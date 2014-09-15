open Core.Std
  
type obj_t = string
    
type context =string 
    
type signature
    
    (* represents how user signs tuples *)
type timestamp
    
type t = string
  
type tuple = Tuple of  t  * t * obj_t * context * signature option  * timestamp option 

type db

let to_string (t:tuple) = 
        match t with
          | t -> ( let Tuple(s, p , o, c, sg , ts) = t in 
                  sprintf "%s %s %s" s p o);;     

let rec print_tuples tuples = 
       match tuples with
       | [] -> print_endline "Finished List"
       |  head::rest -> print_endline (to_string head); print_tuples rest;; 