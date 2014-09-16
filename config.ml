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

let compare t1 t2 = 
  let Tuple(s1, p1 , o1, c1, sg1, ts1) = t1 and  
    Tuple(s2, p2, o2, c2, sg2, ts2) = t2 in
      if s1=s2 || s1 = "*" ||  s2 =  "*" then 
        begin
          if p1=p2 || p1= "*" || p2 = "*" then       
             if o1=o2 || o1= "*" || o2 = "*" then             
               (print_endline "TRUE!"; true)                    
             else false
          else false          
        end
      else 
     (print_endline "Finished FALSE"; false);;
        
                                  