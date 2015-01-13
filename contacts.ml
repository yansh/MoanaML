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
(*** This is an implementation of a sample application - contacts

The main functionality:

* Add contact info
* Query info on the contact
***)
open Config
  
let jon =
  Helper.to_tuple_lst
    "{(a,fn,Jon,contacts)
	  (a,last,Crowcroft,contacts)    
   (a,email,jon.crowcroft@cl.cam.ac.uk,contacts)
	 (a,twitter,@tforcworc,contacts)
	 (a,mobile, 617-000-0001,contacts)
   (a,title,Professor,contacts)	
	 (a,image,jon.jpg, contacts)
	 (a,knows,c,contacts)
	 (a,knows,d,contacts)
	 (a,knows,e,contacts)
	}"
  
let amir =
  Helper.to_tuple_lst
    "{(b,fn,Amir, contacts)
		(b,last,Chaudhry,contacts)    
   (b,email,amir.chaudhry@cl.cam.ac.uk,contacts)
	 (b,twitter,@amirmc,contacts)
	 (b,image,amir.jpg, contacts)
   (b,title,Postdoc,contacts)
   (b,knows,a,contacts)
   (b,knows,c,contacts)
	 (b,knows,d,contacts)
	}"
  
let anil =
  Helper.to_tuple_lst
    "{(c,fn,Anil, contacts)
	  (c,last,Madhavapeddy, contacts)
   (c,email,anil@recoil.org,contacts)    
	 (c,image,anil.jpg, contacts)
	 (c,twitter,@avsm,contacts) 
	 (c,email,anil.madhavapeddy@recoil.org,contacts)
   (c,title,Lecturer,contacts)}"
  
let carlos =
  Helper.to_tuple_lst
    "{(d,fn,Carlos, contacts)
	 (d,last,Molina-jimenez, contacts)
	 (d,image,carlos.jpg, contacts)    
   (d,email,cm770@cam.ac.uk,contacts)        
   (d,title,Postdoc,contacts)
	 (d,twitter,@carlos,contacts) 
	 (d,knows,a,contacts)
	 (d,knows,c,contacts)}"
  
let richard =
  Helper.to_tuple_lst
    "{(e,fn,Richard,contacts)    
		(e,last,Mortier,contacts)
   (e,email,richard.mortier@nottingham.ac.uk,contacts)
	 (e,image,mort.png, contacts)
	 (e,twitter,@mort__,contacts)        
   (e,title,Lecturer,contacts)
	 (e,knows,c,contacts)
	 (e,knows,d,contacts)
	}"
  
let policies =
  Helper.to_tuple_lst "{
	(b,canView,a,policies)
	(c,canView,d,policies)	
	}"
  
let contacts = [ jon; amir; anil; carlos; richard ]

  
(*let contacts = Helper.tuples_from_file "contacts.db"*)
(* sample query *)
let q2 =
  "MAP {
	a,knows,?y,contacts
	?y,fn,?name,contacts
	?y,email,?email,contacts
	}"

  
(* execute policy to bring all the tuples that b can view *)
let q1 =
  "MAP {
	 b, canView,?x, policies
	 ?x, knows, ?o, contacts
	 ?o, email, ?email, contacts	 
	 ?o, fn, ?n, contacts
	}"

(**
 Policy to used as default when the user omits the policy pamateter in
 the query. 
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *) 
let defaultPolicy=  (* q1 *)
  "MAP {
	a,knows,?y,contacts
	?y,fn,?name,contacts
	?y,email,?email,contacts
	}"

(**
 Defaul query variable and context
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *) 
let defaultQvar=  "?y" 
let defaultContx= "contacts"



let prnt() = Helper.print_tuples_list contacts  


(**
 prints the length of the tuple list.
 @author: Carlos Molina-Jimenez 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *) 
let prntlengthofTupleLst() = print_string("\n Results from Helper.prntlengthofTupleLst: ");
           print_int(Helper.lengthofTupleLst contacts);;

(**
 prints the contents of the contact repository.
 @author: Carlos Molina-Jimenez


 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *)
let prntStrTuples() = print_string("\n Results from Helper.listoftuples_to_str: \n");
           print_string(Helper.listoftuples_to_str contacts);;


(**
 returns an integer: the length of the tuple list.
 @author: Carlos Molina-Jimenez 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *) 
let lengthofTupleLst() = Helper.lengthofTupleLst contacts;;


(**
 returns a string: the content of the contact repository.
 @author: Carlos Molina-Jimenez 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *)
let getStrOfTuples() = Helper.listoftuples_to_str contacts;;
  


let results = (Rete.exec_qry q1 (List.flatten contacts)) |> (Rete.exec_bm q2)

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns ...
 pquery: policy query
 qry:    query 
 *)
let qresults pqry qry= (Rete.exec_qry pqry (List.flatten contacts)) |> (Rete.exec_bm qry)

  
(* let (Rete.Node (_, res_bm, _)) = results *)

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns ...
 pquery: policy query
 qry:    query 
 *)

let q3 =
  "MAP {
	d,email,?y,contacts
	}"


(* let (Rete.Node (_, res_bm, _)) = qresults q1 q3 *)
  
(*let p2 =
  Helper.print_tuples (Helper.TupleSet.elements (Rete.get_tuples results))*)
(*let p = Rete.print_bm res_bm*)

let r_map = Rete.get_res_map (qresults q1 q2) [ "?name"; "?y"; "?email" ]

let buildr_map pqry qry= Rete.get_res_map (qresults pqry qry ) ["?name"; "?y"; "?email"]

(*  
 * let _ = Helper.StringMap.iter Helper.print_var r_map
 *) 


(*  
 * Example: 
 * StringMap.iter (fun key value -> Printf.printf " %s: %d\n" key value) h
 * val iter : (key -> 'a -> unit) -> 'a t -> unit
 * val map : ('a -> 'b) -> 'a t -> 'b t
 *)
 let prtvar() = print_string("\n\n Contacts will start execution ...\n\n"); 
         Helper.StringMap.iter Helper.print_var r_map 


(**
 @author: Carlos Molina-Jimenez 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 1) Performs a map operation which results in a list
 [Constant "simon"; Constant "marco"; Constant "Delfi"]
 2) prints the length of the list
 3) prints the head element of the list
 This is only a testing function.
 *) 
let prtvarStr() = print_string("\n\n The output string is: \n\n"); 
    print_string("The len of the list is: ");
    print_int (List.length (Helper.StringMap.find "?name" r_map)); 
    print_string("\n\nValue of the x string at the head of (name, [Cont x; Cont y ]) the list is: ");
    print_string(match List.hd (Helper.StringMap.find "?name" r_map) with
      | Constant x -> x
      | _          -> raise Wrong_value);
     print_newline();;

(**
 @author: Carlos Molina-Jimenez 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 prints the content of a tuple presented as a single string 
 For ex, it converts ?email [Constant simon; Constant marco] 
 into a single string and prints it.
 *)
let prtTuple() =   
    print_string("\n\n The string is: \n\n"); 
    print_string(Helper.tupleToStr "?email" (Helper.StringMap.find "?email" r_map));; 




(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns the content of the whole r_map presented as a 
 single string.
 For ex. give a map, it produces a list like
 [("k1",[Constant "Ali"; Constant "Bob"]); (k2,[Constant "a"; Constant "b"; Constant "c"])],
 conversts it into a single string and prints it.
 *)

let getWhole_r_map() = 
    print_string("\n\n The content of the whole map is: \n\n"); 
    Helper.mapLstToStr (Helper.StringMap.bindings r_map) 


(**
 @author: Carlos Molina-Jimenez 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 prints the content of the whole r_map presented as a 
 single string.
 For ex. give a map, it produces a list like
 [("k1",[Constant "Ali"; Constant "Bob"]); (k2,[Constant "a"; Constant "b"; Constant "c"])],
 conversts it into a single string and prints it.
 *)
let prtWhole_r_map() = 
         print_string("\n\n The content of the whole map is: \n\n"); 
         print_string(Helper.mapLstToStr (Helper.StringMap.bindings r_map));; 


let print_r_map q1 q2 = 
         print_string("\n\n The content of print_r_map is: \n\n"); 
         print_string(Helper.mapLstToStr (Helper.StringMap.bindings (buildr_map q1 q2)));; 

(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string that results from the conversion of the response to a query 
 request (represented by q1) into a single string.
 policy is an optional parameter that defaults to defaultPolicy. 
 *)
 let print_r_map_def_polcy ?policy q1= 
    let plcy= match policy with 
        | None             -> defaultPolicy
        | Some givenPolicy -> givenPolicy   in
        print_string("\n\n Content of print_r_map is: \n\n"); 
        print_string(Helper.mapLstToStr (Helper.StringMap.bindings (buildr_map plcy q1)));; 


(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string that results from the conversion of the response to a query 
 request (represented by subj pred ?qvar ?contx and applied under the
 ?policy) into a single string.
 *)
 let print_r_map_def_plcy ?policy subj pred ?qvar ?contx=
    let plcy= match policy with 
        | None             -> defaultPolicy
        | Some givenPolicy -> givenPolicy   in
    let qryvar= match qvar with
        | None             -> defaultQvar
        | Some givenQvar   -> givenQvar     in
    let context= match contx with
        | None             -> defaultContx
        | Some givenContx  -> givenContx    in 
    let qry = Helper.makeQuery subj pred qryvar context in

     print_string("\n\n Content of print_r_map is: \n\n"); 
     print_string(Helper.mapLstToStr (Helper.StringMap.bindings (buildr_map plcy qry)));; 

(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string that results from the conversion of the response to a query 
 request (represented by subj pred ?qvar ?contx and applied under the
 ?policy) into a single string.
 *)
 let query_r_map_def_plcy ?policy subj pred ?qvar ?contx=
    let plcy= match policy with 
        | None             -> defaultPolicy
        | Some givenPolicy -> givenPolicy   in
    let qryvar= match qvar with
        | None             -> defaultQvar
        | Some givenQvar   -> givenQvar     in
    let context= match contx with
        | None             -> defaultContx
        | Some givenContx  -> givenContx    in 
    let qry = Helper.makeQuery subj pred qryvar context in

    Helper.mapLstToStr (Helper.StringMap.bindings (buildr_map plcy qry)) 
        
 
(**
 @author: Carlos Molina-Jimenez 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *) 
let prtvarString() =   
  print_string("\n\n The first string is: \n\n"); 
  print_string(List.hd (Helper.tupleLstToStrLst "Ali" (Helper.StringMap.find "?name" r_map)));; 
  

(**
 returns a string: a concatenation of the string with itself. I
 used it for testing.
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *)
let duplicatestr s= s^s

(*
 *
 *)
 let runcontacts ()= print_string("\n\n 3Jan: contacts will start execution ...\n\n") 

let hello str= print_string str
let cava str= str 
let hola()= "Olas del mar"


