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
open Yojson
  
exception Wrong_tuple
  
exception Wrong_template
  
exception Wrong_value
  
exception AM_not_found
  
type obj_t = string

type context = string

type signature

(* represents how user signs tuples *)
type timestamp

type t = string

type 't element_type = | Variable of string | Constant of 't

(* Mappings between the different sorts of data. * In this case, since     *)
(* both are represented as strings, * the mapping is the identity          *)
(* function; but the mappings * can be made more complex when the types t  *)
(* and obj_t * are defined further (if they happen to be different).       *)
let t_to_objt : t -> obj_t = fun x -> x
  
let objt_to_t : obj_t -> t = fun x -> x
  
type tuple =
  { subj : t element_type; pred : t element_type; obj : obj_t element_type;
    ctxt : context element_type; time_stp : (timestamp element_type) option;
    sign : (signature element_type) option
  }

type db

(* ---------------- Extended tuple: a tuple extended with a valuation of   *)
(* variables.                                                              *)
type ext_tuple = (((string * (t element_type)) list) * (tuple list))

let execute_query (db : tuple list) (qry : tuple list) : (tuple list) list =
  (* Try to evaluate a variable occurring in query q, occurring * in the   *)
  (* context of var_scope. If the variable doesn't appear * in the scope,  *)
  (* then we simply return the variable. * NOTE Annoyingly in OCaml there  *)
  (* are no first-class selectors, * so we must pass the selector as a     *)
  (* parameter, s in this case.                                            *)
  let try_eval s q var_scope =
    match s q with
    | Variable x ->
        if List.mem_assoc x var_scope then List.assoc x var_scope else s q
    | Constant _ -> s q in
  (* Used to filter a part of a tuple record. * Returns a Boolean,         *)
  (* indicating whether a match has occurred, * and possibly a             *)
  (* variable-value pair (if we're matching against a variable) * -- this  *)
  (* pair is used to extend the valuation which forms part of an *         *)
  (* extended tuple. * s is a record selector * q is a query tuple * v is  *)
  (* a tuple in the database                                               *)
  let sub_filter s q v =
    match s q with
    | (* Remaining variables are treated as wildcards, * but keep the value  *)
        (* the variable evaluates to * since, if this tuple _does_ bring   *)
        (* us closer to * a solution, we'll need to extent var_scope with  *)
        (* * this mapping.                                                 *)
        Variable x -> (true, (Some (x, (s v))))
    | Constant _ -> (((s q) = (s v)), None) in
  let make_query' ((var_scope, pre_soln) : ext_tuple) (q : tuple) :
    ext_tuple list =
    (* q' is a specialised version of q wrt the current pre_soln *)
    let q' =
      {
        subj = try_eval (fun q -> q.subj) q var_scope;
        pred = try_eval (fun q -> q.pred) q var_scope;
        obj = try_eval (fun q -> q.obj) q var_scope;
        ctxt = q.ctxt;(*FIXME ignored for the time being*)
        
        time_stp = q.time_stp;(*FIXME ignored for the time being*)
        
        sign = q.sign;
      }
    in
      (* FIXME ignored for the time being *)
      List.fold_right
        (fun v acc ->
           let (m1, v1) = sub_filter (fun q -> q.subj) q' v in
           let (m2, v2) = sub_filter (fun q -> q.pred) q' v in
           let (m3, v3) = sub_filter (fun q -> q.obj) q' v in
           let scope_ext =
             List.fold_right
               (fun v acc -> match v with | None -> acc | Some x -> x :: acc)
               [ v1; v2; v3 ] []
           in
             (* FIXME remember that we're ignoring ctxt, time_stp and sign *)
             if m1 && (m2 && m3)
             then ((scope_ext @ var_scope), (v :: pre_soln)) :: acc
             else acc)
        db []
  in
    (List.fold_right
       (fun q acc ->
          (List.map (fun ps -> make_query' ps q) acc) |> List.concat)
       qry [ ([], []) ])
      |> (List.map snd)
  
