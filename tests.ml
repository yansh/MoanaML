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

open Config

open OUnit;;
(* Compare the output results with expected results *)

(* let check_test l l1 = let fl = List.flatten l and fl1 = List.flatten l1 *)
(* in let rec compare_list res exp_res = match res, exp_res with [], [] -> *)
(* true | [], _ | _, [] -> false | h::t, h2::t2 -> h=h2 && compare_list t  *)
(* t2 in let c = compare_list fl fl1 in if c=true then print_endline       *)
(* "PASS" else failwith "FAIL";;                                           *)

(***************  TUPLES ***********************)

let t1 = { subj = Constant "a";
	pred = Constant "type";
	obj = Constant "Car";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t2 = { subj = Constant "a";
	pred = Constant "hasColor";
	obj = Constant "c";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t3 = { subj = Constant "b";
	pred = Constant "type";
	obj = Constant "Chair";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t4 = { subj = Constant "b";
	pred = Constant "hasColor";
	obj = Constant "c";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t5 = { subj = Constant "c";
	pred = Constant "type";
	obj = Constant "Color";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t6 = { subj = Constant "c1";
	pred = Constant "type";
	obj = Constant "Color";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t7 = { subj = Constant "c";
	pred = Constant "rgbValue";
	obj = Constant "White";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t8 = { subj = Constant "c1";
	pred = Constant "rgbValue";
	obj = Constant "Red";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t9 = { subj = Constant "a";
	pred = Constant "hasColor";
	obj = Constant "Red";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t10 = { subj = Constant "d";
	pred = Constant "relatesTo";
	obj = Constant "c";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let t11 = { subj = Constant "c";
	pred = Constant "relatesTo";
	obj = Constant "d";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let tuples = [t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t11];;

(* Config.print_tuples tuples;; *)

(*************** QUERY TEMPLATE TUPLES *******************)

let q1 = { subj = Variable "?x";
	pred = Constant "type";
	obj = Constant "Car";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q2 = { subj = Variable "?x";
	pred = Constant "hasColor";
	obj = Constant "Red";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q3 = { subj = Variable "?x";
	pred = Constant "hasColor";
	obj = Variable "?y";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q4 = { subj = Variable "?y";
	pred = Constant "type";
	obj = Constant "Color";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q5 = { subj = Variable "?y";
	pred = Constant "rgbValue";
	obj = Constant "White";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q6 = { subj = Variable "?y";
	pred = Constant "relatesTo";
	obj = Variable "?x";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

let q7 = { subj = Variable "?x";
	pred = Constant "relatesTo";
	obj = Variable "?y";
	ctxt = Constant "context";
	time_stp = None;
	sign = None };;

(****************** TESTS **********************************)

   
print_endline "---- Unit tests ----- ";;

(* TEST 1:
   
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
     
*)



let test1 _ =
  let db = Moana_lists.S.init tuples in
  let query1 = [q1;q2]
  and q1_exp_res = [[t1;t9]] in
  let res_q1 = Moana_lists.S.query db query1 in
  assert_equal q1_exp_res res_q1;;
(*print_tuples_list res_q1;;
 print_tuples_list q1_exp_res;;*)


(* TEST 2:
   
 MAP  {    
     ?x, type, Car
     ?x, color, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }test1
     
*)

let test2 _ = 
  let query2 = [q1;q3;q4;q5] and q2_exp_res = [[t1;t2;t5;t7]] in 
     let res_q2 = execute_query tuples query2 in assert_equal q2_exp_res res_q2 ;;


(* TEST 3: similar to test 1 query but the order of the tuple template is different
   
 MAP  {    
     ?y, rgbValue, White
     ?x, type, Car
     ?x, color, ?y
     ?y, type, Color  
    }
     
*)

let test3 _ = 
  let query3 = [q5;q1;q3;q4] and q3_exp_res = [[t7;t1;t2;t5]] in 
  let res_q3 = execute_query tuples query3  in  assert_equal q3_exp_res res_q3 ;;
  

(*print_tuples_list res_q2;;
   print_tuples_list q2_exp_res;;*)



(* TEST 4: - Note - expected results are order sensetive
   
 MAP  {    
     ?x, relatesTo, ?y
     ?y, relatesTo, ?x
    }
     
*)
let test4 _ = 
  let query4 = [q6;q7] and q4_exp_res = [[t11;t10];[t10;t11]] in 
  let res_q4 = execute_query tuples query4 in assert_equal q4_exp_res res_q4 ;;



(******* based on LUBM benchmark for SPARQL queries **** 
 *  http://swat.cse.lehigh.edu/projects/lubm/queries-sparql.txt
 *)


(* TODO TEST  :
   
MAP {
  ?X type GraduateStudent,
  ?X takesCourse GraduateCourse0}
     
*)


(* TODO TEST :
   
MAP  {    
  X type GraduateStudent,
  ?Y type University,
  ?Z type Department,
  ?X memberOf ?Z,
  ?Z subOrganizationOf ?Y,
  ?X undergraduateDegreeFrom ?Y 
  }
     
*)

(* TODO TEST :
   
MAP  {    
  ?X type Student,
  ?Y type Faculty,
  ?Z type Course,
  ?X advisor ?Y,
  ?Y teacherOf ?Z,
  ?X takesCourse ?Z,
}
     
*)

(*  test filter function *)
let test5 _ =
  let query5= q6 and q5_exp_res=[t10;t11] in
  let res_q5 = filter query5 tuples in assert_equal q5_exp_res res_q5 ;;

(* create alpha memory and add tuples to it *)
let test6 _ =
    let query6 = q6 and q6_exp_res =[t10;t11] in
    let am = create_am query6 tuples in (*let p = 
		List.map (fun l -> match l with	  
					| (var, values) -> print_string var; 
					List.map (fun value -> match value with
						| Constant x, t -> print_endline ""; print_string x;  print_endline (to_string t);
						| Variable _, t-> print_string " ") values) am.vars in*) assert_equal q6_exp_res am.tuples;;

(* test simple join function AM with empty BM *)


let test7 _ =     
	let q7_exp_res ={solutions=[("?x",(Constant "a",[t1]))]} in
  let am = create_am q1 tuples in let bm = {solutions =[]} in 
	let new_bm =join am bm in 
   (* let p =List.map (fun l -> (* (string * (t element_type * tuple list) ) *)						
		match l with
		| var, (value, tuples) -> print_endline var; print_value value;
              print_string "["; 
                        List.map (fun t -> print_string (to_string t)) tuples )   new_bm.solutions  
												in *)  assert_equal new_bm q7_exp_res;; 

(* TEST :
 
Implement by create a combination of AM and BM
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
*)
let test8 _ =         
  let am1 = create_am  q1 tuples and am2 = create_am q2 tuples and  q8_exp_res ={solutions=[("?x",(Constant "a",[t9;t1]))]} in let bm = {solutions =[]} in 
    let res_bm =join am2 (join am1 bm) in 
    (*let p =List.map (fun l -> (* (string * (t element_type * tuple list) ) *)                     
        match l with
        | var, (value, tuples) -> print_endline var; print_value value;
              print_string "["; 
                        List.map (fun t -> print_string (to_string t)) tuples ) res_bm.solutions in 
												*)assert_equal res_bm  q8_exp_res;; 

let suite =
	"Unit tests" >:::
	[ "test1" >:: test1;
	"test2" >:: test2;
	"test3" >:: test3;
	"test4" >:: test4;
	"test5" >:: test5;
	"test6" >:: test6;
	"test7" >:: test7;
	"test8" >:: test8;]

let _ = run_test_tt_main suite
