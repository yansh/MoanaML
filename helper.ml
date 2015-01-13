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
  
open Yojson
  
(** tuple object to string **)
let to_string =
  function
  | {
      subj = Constant s;
      pred = Constant p;
      obj = Constant o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ } -> Printf.sprintf "< %s %s %s >" s p o
  | _ -> "Not printing this tuple."
  
let value_to_str = function | Variable x -> x | Constant x -> x
  
(** compare two tuples **)
let ( = ) t1 t2 =
  let s1 = to_string t1 in let s2 = to_string t2 in String.compare s1 s2
  
module TupleSet = Set.Make(struct type t = tuple

                                   let compare = ( = )
                                      end)
  
module StringMap =
  Map.Make(struct type t = string

                   let compare = String.compare
                      end)

  
(* FIX ME: Need to take care of Context, Signature and Timestamp *)
let to_json =
  function
  | {
      subj = Constant s;
      pred = Constant p;
      obj = Constant o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ } ->
      let t_json : Yojson.Basic.json =
        `Assoc
          [ ("Subject", (`String s)); ("Predicate", (`String p));
            ("Object", (`String o)) ]
      in t_json
  | _ -> assert false
  
(* create tuple from JSON *)
let from_json json_t =
  (* FIX ME: Need to take care of Context, Signature and Timestamp *)
  let open Yojson.Basic.Util
  in
    let json = Yojson.Basic.from_string json_t in
    let s = json |> (member "Subject") in
    let p = json |> (member "Predicate") in
    let o = json |> (member "Object")
    in
      {
        subj = Constant (Basic.Util.to_string s);
        pred = Constant (Basic.Util.to_string p);
        obj = Constant (Basic.Util.to_string o);
        ctxt = Constant "context";
        time_stp = None;
        sign = None;
      }
  
let print_value =
  function
  | Variable x ->
      (print_string "Var (";
       print_string x;
       print_string ") ";
       print_endline "")
  | Constant x ->
      (print_string "Const (";
       print_string x;
       print_string ") ";
       print_endline "")

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Converts a single tuple like < c fn Anil> < c last Madhavapeddy> ...
 < c title Lecturer > into a string and returns the resulting string.
 *)
let tupleToStr tuple=
  let rec help str tuple= match tuple with
  | []      -> str^"\n--nextContact--\n"
  | h::rest -> help (str^(to_string h)^"\n") rest 
  in help "" tuple

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns an integer: the length of the list which contains 
 strings that represent tuples
 *)
 let lengthofTupleLst tupleLst = 
    let lst= List.map (fun t -> tupleToStr t) tupleLst in
    List.length lst

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: the results of converting a list of strings 
 like [s1;s2;s3] into a single string s= s1^s2^s3 
 *)
 let listofStrToStr lst= 
     let rec help str lst= match lst with
     | []      -> str^"\nNo more contacts\n"
     | h::rest -> help (str^h) rest 
     in help "" lst

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: the result of converting a list of tuples 
 into a string
 *)
 let listoftuples_to_str tuples= 
    let lst= List.map (fun t -> tupleToStr t) tuples in
    listofStrToStr lst


(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: a query to be presented to a rete map. 
 *) 
 let makeQuery subj pred qryvar context=
    "MAP{" ^ subj ^ "," ^ pred ^ "," ^ qryvar ^ "," ^ context ^ "}";;

(****************)

(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string: the result of converting
 Constant str into str
 *)
let constStrToStr constStr= match constStr with
    | Constant value -> "\n"^value
    | _              -> raise Wrong_value


(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a list of stringis: the result of converting a tuple 
 (a key value pair) like "simon" [Constant "marco"; Constant "defino"]
 into a list like ["simon: "; "marco"; "delfino"]
 *)
let tupleLstToStrLst var lst= 
    let resLst= List.map constStrToStr lst in
    (var^":")::resLst

(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string: the result of converting a tuple 
 (a key value pair) like "simon" [Constant "marco"; Constant "defino"]
 into a string like "simon: marco delfino"
 *)
let tupleToStr var lst= 
    let resLst= List.map constStrToStr lst in
    listofStrToStr((var^":")::resLst)

let tupleToString tuple=
    let var= fst(tuple) in
    let lst= snd(tuple) in
    tupleToStr var lst

let mapLstToStr mapLst= listofStrToStr (List.map tupleToString mapLst) 



let rec print_tuples tuples =
  match tuples with
  | [] -> print_endline "--"
  | head :: rest -> (print_endline (to_string head); print_tuples rest)
  
let print_tuples_list tuples = List.map (fun t -> print_tuples t) tuples
 

 
(* helper function for printing out (variable, values) pairs *)
(* 
 *var is a string, for example "abc", "?email", etc.
 *x is a string: Anil, Carlos, cm770@cam.ac.uk, a, b ,c
 *)

let print_var var values =
  (print_endline var;
   List.iter
     (fun vl ->
        match vl with
        | Constant x -> print_endline x
        | _ -> raise Wrong_value)
     values)


let valuesToStr_var var values =
  let lst= List.map
     (fun vl ->
        match vl with
        | Constant x ->  x
        | _ -> raise Wrong_value)
     values in 
   var^" ..."^(List.hd lst)  
 
(* let lstResultsToStr lst= *)
  
 
open Lexing
  
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p
  in
    Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
      ((pos.pos_cnum - pos.pos_bol) + 1)
  
(** convert string to list of tuple objects **)
exception SyntaxError of string
  
(** gen tuples from a file **)
let tuples_from_file file =
let ch = open_in file in      
     let lexbuf = Lexing.from_channel ch   
		in
    try Tuple_parser.parse_collection Tuple_lexer.lex lexbuf;
	
    with
    | SyntaxError msg ->
        (Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg; [])
    | Tuple_parser.Error ->
        (Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
         [])

 
let to_tuple_collection s =
  let lexbuf = Lexing.from_string s
  in
    try Tuple_parser.parse_collection Tuple_lexer.lex lexbuf
    with
    | SyntaxError msg ->
        (Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg; [])
    | Tuple_parser.Error ->
        (Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
         [])	 
let to_tuple_lst s =
  let lexbuf = Lexing.from_string s
  in
    try Tuple_parser.parse_lst Tuple_lexer.lex lexbuf
    with
    | SyntaxError msg ->
        (Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg; [])
    | Tuple_parser.Error ->
        (Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
         [])
  
(** string to tuple *)
let to_tuple s =
  let tuple = Tuple_parser.parse_tuple Tuple_lexer.lex (Lexing.from_string s)
  in match tuple with | Some t -> t | None -> raise Wrong_tuple
  
(** string to list of query tuples **)
let str_query_list s =
  let queryBuffer = Lexing.from_string s
  in Query_parser.parse Query_lexer.lex queryBuffer
  
(** flatten list of unique tuples **)
let flatten_tuple_list tuples =
  let tuples_set = TupleSet.empty
  in
    (List.fold_right
       (fun tuples acc -> List.fold_right TupleSet.add tuples acc) tuples
       tuples_set)
      |> TupleSet.elements
  
