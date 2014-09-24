(* Compare the output results with expected results *)

open Config

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


