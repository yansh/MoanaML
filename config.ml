open Core.Std
  
type obj_t = string
    
type context
    
type signature
    
    (* represents how user signs tuples *)
type timestamp
    
type t = string
  
type tuple = Tuple of  t  * t * obj_t * t * signature option  * timestamp option 

type db