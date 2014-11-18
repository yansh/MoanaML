(* * Copyright (c) 2014 Yan Shvartzshnaider * * Permission to use, copy,   *)
(* modify, and distribute this software for any * purpose with or without  *)
(* fee is hereby granted, provided that the above * copyright notice and   *)
(* this permission notice appear in all copies. * * THE SOFTWARE IS        *)
(* PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES * WITH REGARD  *)
(* TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF * MERCHANTABILITY  *)
(* AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR * ANY SPECIAL,  *)
(* DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES * WHATSOEVER  *)
(* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN * ACTION OF  *)
(* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF * OR IN   *)
(* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                *)
(*** This is an implementation of a sample application - contacts

The main functionality:

* Add contact info
* Query info on the contact
***)
open Config 

let jon =
  Helper.to_tuple_lst
    "{(a,fn,Jon Crowcroft,contacts)    
   (a,email,jon.crowcroft@cl.cam.ac.uk,contacts)
	 (a,twitter,@tforcworc,contacts)
   (a,title,Professor,contacts)
	 (a,knows,b,contacts)
	 (a,knows,c,contacts)
	 (a,knows,d,contacts)
	 (a,knows,e,contacts)
	}"
  
let amir =
  Helper.to_tuple_lst
    "{(b,fn,Amir Chaudhry, contacts)    
   (b,email,amir.chaudhry@cl.cam.ac.uk,contacts)
	 (b,twitter,@amirmc,contacts)
   (b,title,Postdoc,contacts)
   (b,knows,a,contacts)
   (b,knows,c,contacts)
	 (b,knows,d,contacts)
	}"
  
let anil =
  Helper.to_tuple_lst
    "{(c,fn,Anil Madhavapeddy, contacts)    
   (c,email,anil@recoil.org,contacts)    
	 (c,email,anil.madhavapeddy@recoil.org,contacts)
   (c,title,Lecturer,contacts)}"
  
let carlos =
  Helper.to_tuple_lst
    "{(d,fn,Carlos Molina-jimenez, contacts)    
   (d,email,cm770@cam.ac.uk,contacts)        
   (d,title,Postdoc,contacts)
	 (d,knows,a,contacts)
	 (d,knows,c,contacts)}"
  
let richard =
  Helper.to_tuple_lst
    "{(e,fn,Richard Mortier,contacts)    
   (e,email,richard.mortier@nottingham.ac.uk,contacts)
	 (e,twitter,@mort__,contacts)        
   (e,title,Lecturer,contacts)
	 (e,knows,a,contacts)
	 (e,knows,c,contacts)
	 (e,knows,d,contacts)
	}"
  
let contacts = [ jon; amir; anil; carlos; richard ]

(* sample query *)  
let q1 = "MAP {
	?y,knows,a,contacts
	?y,fn,?name,contacts
	?y,email,?email,contacts
	}"
  
let results = Rete.exec_qry q1 contacts "?email" |> List.iter (fun v -> print_endline v)

