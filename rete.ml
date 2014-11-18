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
(* Irmin based storage *)
(* ------ RETE ------------ *)
(** AM keeps tuples matching the pattern.
* Each AM also contains vars which is a list of paits (position, var_string)
* position stands for the position of varibale in a pattern and var_string denotes its string value.
* This is used later when we join AM with BM
* 
*)
open Config
  
(* helper to filter tuples list to form the pattern *)
let filter ptrn tuples =
  let cmp p_attr t_attr =
    match p_attr with | Variable _ -> true | Constant _ -> p_attr = t_attr
  in
    List.filter
      (fun t ->
         (cmp ptrn.subj t.subj) &&
           ((cmp ptrn.pred t.pred) && (cmp ptrn.obj t.obj)))
      tuples
  
(* add value to the list of values associated with the variable *)
let sel_arg arg pos =
  match (arg, pos) with
  | (Constant _, _) -> None
  | (Variable var, pos) -> Some (var, pos)
  
let mappings p tuples =
  List.fold_right
    (fun e acc ->
       match e with
       | Some (var, 1) ->
           acc @ [ (var, (List.map (fun t -> ((t.subj), t)) tuples)) ]
       | Some (var, 2) ->
           acc @ [ (var, (List.map (fun t -> ((t.pred), t)) tuples)) ]
       | (*acc @ [(var, List.map (fun t ->  print_value t.pred; t.pred) tuples)]*)
           Some (var, 3) ->
           acc @ [ (var, (List.map (fun t -> ((t.obj), t)) tuples)) ]
       | Some (_, _) -> acc
       | None -> acc)
    [ sel_arg p.subj 1; sel_arg p.pred 2; sel_arg p.obj 3 ] []
  
type am =
  { tuples : tuple list; pattern : tuple;
    (* this is a mapping of Variables and their respective values in each tuple *)
    vars : (string * (((t element_type) * tuple) list)) list
  }

(* helper to print the mappings *)
let print_mappings am =
  List.map
    (fun (var, values) ->
       (print_string var;
        List.map
          (fun value ->
             match value with
             | (Constant x, t) ->
                 (print_endline "";
                  print_string x;
                  print_endline (Helper.to_string t))
             | (Variable _, _) -> print_string " ")
          values))
    am.vars
  
let create_am p tuples_ =
  {
    pattern = p;
    tuples = filter p tuples_;
    vars =
      if (List.length tuples_) > 0 then mappings p (filter p tuples_) else [];
  }
  
(* BM contains (var, value, solution for the value *)
type bm = { solutions : (string * ((t element_type) * (tuple list))) list }

(* helper to print BM *)
let print_bm bm =
  List.map
    (fun (var, (value, tuples)) ->
       (* (string * (t element_type * tuple list) ) *)
       (print_endline "";
        print_endline var;
        Helper.print_value value;
        print_string "[";
        List.map (fun t -> print_string (Helper.to_string t)) tuples))
    bm.solutions
  
(* joining BM and AM to create a new BM *)
let join am bm =
  {
    solutions =
      match bm with
      | { solutions = [] } ->
          List.fold_right
            (fun (var, values) acc ->
               (*string * ((t element_type * tuple) list)*)
               (* am: (t element_type * tuple list) *)
               acc @
                 (List.map (fun (value, tuple) -> (var, (value, [ tuple ])))
                    values))
            am.vars []
      | { solutions = solutions } ->
          (* (string * (t element_type * tuple list) ) list * -- existing  *)
          (* solution                                                      *)
          List.fold_right (* string * ((t element_type * tuple) list) *)
            (* am: (t element_type * tuple) list) *)
            (fun (am_var, am_values) acc ->
               try
                 let (bm_value, _) = List.assoc am_var solutions in
                 (* filter all the solutions assoc with the variable *)
                 let sol_list =
                   List.filter (fun (bm_var, _) -> bm_var = am_var) solutions in
                 let sol =
                   List.fold_right
                     (fun (_, (bm_value, sol_tuples)) acc_f ->
                        (* filter tuples that have matching values *)
                        (* to corresponding variable               *)
                        let fltr_list =
                          List.filter (fun (value, _) -> value = bm_value)
                            am_values
                        in
                          acc_f @
                            (List.map
                               (fun (_, tuple) ->
                                  (am_var, (bm_value, (tuple :: sol_tuples))))
                               fltr_list))
                     sol_list []
                 in sol
               with
               | (*In a nutshell, when a variable from an AM is not found in BM solution set *)
                   (* we apply_ptrn to find values for other variable in the tuple. *)
                   (* Eg., in case we have pattern ?x type ?y and ?x is not found in BM solutions *)
                   (* the we check the value for ?y and see if ?y appears in the BM, if does we add the*)
                   (*  tuple to the solution *) Not_found ->
                   let apply_ptrn p tuple =
                     List.fold_right
                       (fun e acc ->
                          match e with
                          | Some (var, 1) ->
                              if var <> am_var
                              then acc @ [ (var, ((tuple.subj), tuple)) ]
                              else acc
                          | Some (var, 2) ->
                              if var <> am_var
                              then acc @ [ (var, ((tuple.pred), tuple)) ]
                              else acc
                          | Some (var, 3) ->
                              if var <> am_var
                              then acc @ [ (var, ((tuple.obj), tuple)) ]
                              else acc
                          | Some (_, _) -> acc
                          | None -> acc)
                       [ sel_arg p.subj 1; sel_arg p.pred 2; sel_arg p.obj 3 ]
                       []
                   in
                     acc @
                       (List.fold_right
                          (fun (am_value, tuple) acc1 ->
                             acc1 @
                               (List.fold_right
                                  (fun (var, (value, tuple)) acc2 ->
                                     (* filter all the solutions assoc with the variable *)
                                     let sol_list =
                                       List.filter
                                         (fun
                                            (bm_var, (bm_value, sol_tuples))
                                            ->
                                            (bm_var = var) &&
                                              (bm_value = value))
                                         solutions
                                     in
                                       List.map
                                         (fun
                                            (bm_var, (bm_value, sol_tuples))
                                            ->
                                            (am_var,
                                             (am_value,
                                              (tuple :: sol_tuples))))
                                         sol_list)
                                  (apply_ptrn am.pattern tuple) []))
                          am_values []))
            am.vars [];
  }
  
(*in
  let p = print_endline "BM-->" in
  let p2 = print_bm b in let p = print_endline "<---" in b*)
type rete_dataflow = | Empty | Node of am * bm * rete_dataflow

let rete ams =
  let first_am = List.hd ams in
  let empty_bm = { solutions = []; } in
  let tail = List.tl ams in
  (*let p = print_bm (join first_am empty_bm)in let p1= print_endline "+++" in*)
  let res_list =
    List.fold_right
      (fun am acc ->
         let (_, prev_bm) = List.hd acc in (am, (join am prev_bm)) :: acc)
      (List.rev tail) [ (first_am, (join first_am empty_bm)) ]
  in List.fold_right (fun (am, bm) acc -> Node (am, bm, acc)) res_list Empty
  
(*** generate a list of (AM, ref rete_node) pairs ***)
let rec mk_refs rete_network acc =
  match rete_network with
  | Node (am, _, node) -> (am, (ref rete_network)) :: (mk_refs node acc)
  | Empty -> acc
  
(** add tuple to an existing AM **)
(* FIX ME: remove AM from the parameters, replace by matching tuple to the AM *)
(* FIX ME: implement efficient way to create new AM from the old one *)
let add rete_network am tuple =
  let node_refs = mk_refs rete_network []
  in
    try
      let node_ref = List.assoc am node_refs in
      let rec regen rete_network =
        let get_bm node =
          match node with
          | Node (_, bm, _) -> bm
          | Empty -> { solutions = []; }
        in
          match rete_network with
          | Node (current_am, bm, node) ->
              if rete_network <> !node_ref
              then (*let p= print_bm  bm in*)
                Node (current_am, (join current_am (get_bm (regen node))),
                  node)
              else
                (let new_am =
                   create_am current_am.pattern (tuple :: current_am.tuples)
                 in
                   (*let p = print_bm (join new_am (get_bm node)) in*)
                   (node_ref :=
                      Node (new_am, (join new_am (get_bm node)), node);
                    !node_ref))
          | Empty -> Empty
      in regen rete_network
    with
    | (*let (_, p_node) = List.hd node_refs in execute_rete !p_node*)
        Not_found -> Empty
  
(*** given rete network start activations **)
let rec execute_rete rete_network =
  let get_bm node =
    match node with | Node (_, bm, _) -> bm | Empty -> { solutions = []; }
  in
    match rete_network with
    | Node (am, _, node) ->
        Node (am, (join am (get_bm node)), (execute_rete node))
    | Empty -> Empty
  
(* takes a list of AMs and joins them *)
let execute_am_list ams =
  let empty_bm = { solutions = []; }
  in List.fold_right (fun am acc -> join am acc) ams empty_bm
  
(** function to create rete newtork from a query **)
let to_rete str tuples =
  let qs = Helper.str_query_list str in
  let ams = List.map (fun q -> create_am q tuples) qs in rete ams
  
 
(** function to return a list of values for a particular variable in the solution (BM) **)
let get_lst_values bm var = (* helper to print BM *)
  List.fold_right
    (fun (v, (value, tuples)) acc ->
       (* (string * (t element_type * tuple list) ) *)
       if var = v then value :: acc else acc)
    bm.solutions []
		
(** helper method accepts query string and runs it over tuples, 
extracts the values associated with the var **)
  
let exec_qry q tuples var =  
  let network = to_rete q (List.flatten tuples) in
  let result =
    match execute_rete network with
    | Node (_, result, _) -> result
    | Empty -> { solutions = []; } in
  (*let p = Rete.print_bm result*)
  let v_list = get_lst_values result var
  in
    List.map
      (function
       | Constant x -> x
       | _ -> raise Wrong_value )
      v_list
  
