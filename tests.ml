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


open Config
(* Compare the output results with expected results *)

let check_test l  l1  =
  let fl = List.flatten l and fl1 = List.flatten l1 in  
      let rec compare_list res exp_res = 
        match res, exp_res with
          [], [] -> true
          | [], _
          | _,  [] -> false
          | h::t, h2::t2 ->  h=h2 && compare_list t t2  in         
   let c = compare_list fl fl1  in                    
      if c=true then 
        print_endline "PASS"
      else            
        failwith "FAIL";;

(***************  TUPLES ***********************)



let t1 = {subj = Constant "a";
          pred = Constant "type";
          obj = Constant  "Car";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
            

let t2 = {subj = Constant "a";
          pred = Constant "hasColor";
          obj = Constant  "c";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t3 = {subj = Constant "b";
          pred = Constant "type";
          obj = Constant  "Chair";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
          
let t4 = {subj = Constant "b";
          pred = Constant "hasColor";
          obj = Constant  "c";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;


let t5 = {subj = Constant "c";
          pred = Constant "type";
          obj = Constant  "Color";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t6 = {subj = Constant "c1";
          pred = Constant "type";
          obj = Constant  "Color";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
          

let t7 = {subj = Constant "c";
          pred = Constant "rgbValue";
          obj = Constant  "White";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t8 = {subj = Constant "c1";
          pred = Constant "rgbValue";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t9 = {subj = Constant "a";
          pred = Constant "hasColor";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let tuples = [t1;t2;t3;t4;t5;t6;t7;t8;t9];;

(*Config.print_tuples tuples;;*)


(*************** QUERY TEMPLATE TUPLES *******************)

let t1 = {subj = Constant "a";
          pred = Constant "type";
          obj = Constant  "Car";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
            

let t2 = {subj = Constant "a";
          pred = Constant "hasColor";
          obj = Constant  "c";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t3 = {subj = Constant "b";
          pred = Constant "type";
          obj = Constant  "Chair";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
          
let t4 = {subj = Constant "b";
          pred = Constant "hasColor";
          obj = Constant  "c";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;


let t5 = {subj = Constant "c";
          pred = Constant "type";
          obj = Constant  "Color";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t6 = {subj = Constant "c1";
          pred = Constant "type";
          obj = Constant  "Color";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;
          

let t7 = {subj = Constant "c";
          pred = Constant "rgbValue";
          obj = Constant  "White";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t8 = {subj = Constant "c1";
          pred = Constant "rgbValue";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let t9 = {subj = Constant "a";
          pred = Constant "hasColor";
          obj = Constant  "Red";
          ctxt = Constant "context";
          time_stp =None;
          sign = None};;

let tuples = [t1;t2;t3;t4;t5;t6;t7;t8;t9];;

(*Config.print_tuples tuples;;*)


(*************** QUERY TEMPLATE TUPLES *******************)

let q1 = {subj = Variable "?x";
          pred = Constant "type";
          obj =  Constant "Car";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let q2 = {subj = Variable "?x";
          pred = Constant "hasColor";
          obj =  Constant "Red";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let q3 = {subj = Variable "?x";
          pred = Constant "hasColor";
          obj =  Variable "?y";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let q4 = {subj = Variable "?y";
          pred = Constant "type";
          obj =  Constant "Color";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;

let q5 = {subj = Variable "?y";
          pred = Constant "rgbValue";
          obj =  Constant "White";
          ctxt = Constant "context";
          time_stp = None;
          sign = None};;


(****************** TESTS **********************************)

   
print_endline "---- Unit tests ----- ";;

(* TEST 1:
   
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
     
*)
let test1 = (print_endline "Test 1") in 
  let query1 = [q1;q2] and q1_exp_res = [[t1;t9]] in
  let res_q1 = execute_query tuples query1 in check_test q1_exp_res res_q1;; 
(*print_tuples_list res_q1;;
 print_tuples_list q1_exp_res;;*)


(* TEST 2:
   
 MAP  {    
     ?x, type, Car
     ?x, color, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }
     
*)

let test2 = (print_endline "Test 2") in 
  let query2 = [q1;q3;q4;q5] and q2_exp_res = [[t1;t2;t5;t7]] in 
let res_q2 = execute_query tuples query2 in check_test q2_exp_res res_q2 ;;

(*print_tuples_list res_q2;;
   print_tuples_list q2_exp_res;;*)
