open Yojson
  
exception Wrong_tuple
  
exception Wrong_template
  
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
  
(* FIX ME: Need to take care of Context, Signature and Timestamp *)
let to_json t =
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
  
let rec print_tuples tuples =
  match tuples with
  | [] -> print_endline "--"
  | head :: rest -> (print_endline (to_string head); print_tuples rest)
  
let print_tuples_list tuples = List.map (fun t -> print_tuples t) tuples
  
(* ------ RETE ------------ *)
(** AM keeps tuples matching the pattern.
* Each AM also contains vars which is a list of paits (position, var_string)
* position stands for the position of varibale in a pattern and var_string denotes its string value.
* This is used later when we join AM with BM
*
* TODO: add a list of join nodes AM is connected to, to perform (right) activation when a new tuple is added
*)
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
                  print_endline (to_string t))
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
        print_value value;
        print_string "[";
        List.map (fun t -> print_string (to_string t)) tuples))
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
                   create_am current_am.pattern (tuple :: current_am.tuples) in
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
  
