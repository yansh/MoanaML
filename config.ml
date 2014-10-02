exception Wrong_tuple
exception Wrong_template

type obj_t = string

type context = string

type signature

(* represents how user signs tuples *)
type timestamp

type t = string

type 't element_type =
		Variable of string
	| Constant of 't

(* Mappings between the different sorts of data. * In this case, since     *)
(* both are represented as strings, * the mapping is the identity          *)
(* function; but the mappings * can be made more complex when the types t  *)
(* and obj_t * are defined further (if they happen to be different).       *)
let t_to_objt : (t -> obj_t) = fun x -> x
let objt_to_t : (obj_t -> t) = fun x -> x

type tuple =
	{ subj : t element_type;
		pred : t element_type;
		obj : obj_t element_type;
		ctxt : context element_type;
		time_stp : timestamp element_type option;
		sign : signature element_type option }

type db

let to_string t =
	match t with
		{ subj = Constant(s);
		pred = Constant(p);
		obj = Constant(o);
		ctxt = Constant(c);
		time_stp = _ ;
		sign = _ }-> Printf.sprintf "< %s %s %s >" s p o
	| _ -> "Not printing this tuple." ;;

(* ------ RETE ------------ *)

(** AM keeps tuples matching the pattern.
* Each AM also contains vars which is a list of paits (position, var_string)
* position stands for the position of varibale in a pattern and var_string denotes its string value.
* This is used later when we join AM with BM
*
* TODO: add a list of join nodes AM is connected to, to perform (right) activation when a new tuple is added
*)

(* helper to filter tuples list to form the pattern *)

let filter ptrn (tuples: tuple list) =
	let cmp p_attr t_attr =
		match p_attr with
			Variable _ -> true
		| Constant _ -> p_attr = t_attr
	in List.filter (fun t -> (cmp ptrn.subj t.subj && cmp ptrn.pred t.pred && cmp ptrn.obj t.obj)) tuples ;;

(* This is a helper function to generate pos and Var string from a pattern *)

let mappings p =
	let sel_arg arg pos =
		match arg, pos with
			Constant _, _ -> None
		| Variable x, pos -> Some (x, pos) in
	let get = function
			None -> failwith "No option value"
		| Some x -> x in
	(List.fold_right (fun e acc ->
						if e <> None
						then [get e] @ acc
						else acc) [sel_arg p.subj 1; sel_arg p.pred 2; sel_arg p.obj 3] [] ) ;;

type am = { tuples: tuple list; pattern: tuple; vars : (string * int) list }

let create_alpha p tuples = { pattern = p; tuples = filter p tuples; vars = mappings p }
(* add tuples that match the pattern to the AM *)
let add_to_alpha am tpls = { pattern = am.pattern; tuples = List.append am.tuples (filter am.pattern tpls); vars = am.vars }

(* beta memory contains a result from joining with alpha memory each tuple *)
(* in the BM is associated with binding variable with (its position in the *)
(* tuple, and                                                              *)

type bm = { tuples: ((string*int)*tuple) list list; vars : (string * int) list }

let rec print_tuples tuples =
	match tuples with
	| [] -> print_endline "--"
	| head:: rest -> print_endline (to_string head); print_tuples rest;;

let print_tuples_list tuples =
	List.map (fun t -> print_tuples t) tuples;;

(*-------------------------------------*)

(* HARDCODED QUEY MAP ?x { ?x type ?y ?x color, Red } *)
let qry2 l =
	let l1 = List.filter (fun tup ->
						tup.pred = Constant "type") l in (* all elements of predicate type *)
	let l2 = List.filter (fun tup ->
						tup.pred = Constant "hasColor" && tup.obj = Constant "Red") l in (* all elements that have color Red *)
	List.fold_right (fun tup1 acc ->
					let join = List.filter (fun tup2 ->
										tup1.subj = tup2.subj) l2 in
					if join = [] then acc else tup1 :: join @ acc) l1 [];;
(*  ---------------- *)

(* Extended tuple: a tuple extended with a valuation of variables. *)
type ext_tuple = (string * t element_type) list * tuple list


let execute_query (db : tuple list) (qry : tuple list) : tuple list list =
	(* Try to evaluate a variable occurring in query q, occurring * in the     *)
	(* context of var_scope. If the variable doesn't appear * in the scope,    *)
	(* then we simply return the variable. * NOTE Annoyingly in OCaml there    *)
	(* are no first-class selectors, * so we must pass the selector as a       *)
	(* parameter, s in this case.                                              *)
	let try_eval s q var_scope =
		match s q with
		| Variable x ->
				if List.mem_assoc x var_scope then
					List.assoc x var_scope
				else s q
		| Constant _ -> s q in
	(* Used to filter a part of a tuple record. * Returns a Boolean,           *)
	(* indicating whether a match has occurred, * and possibly a               *)
	(* variable-value pair (if we're matching against a variable) * -- this    *)
	(* pair is used to extend the valuation which forms part of an * extended  *)
	(* tuple. * s is a record selector * q is a query tuple * v is a tuple in  *)
	(* the database                                                            *)
	let sub_filter s q v =
		match s q with
		(* Remaining variables are treated as wildcards, * but keep the value the  *)
		(* variable evaluates to * since, if this tuple _does_ bring us closer to  *)
		(* * a solution, we'll need to extent var_scope with * this mapping.       *)
		| Variable x -> (true, Some (x, s v))
		| Constant _ -> (s q = s v, None) in
	let make_query' ((var_scope, pre_soln) : ext_tuple) (q : tuple) : ext_tuple list =
		(* q' is a specialised version of q wrt the current pre_soln *)
		let q' =
			{ subj = try_eval (fun q -> q.subj) q var_scope;
				pred = try_eval (fun q -> q.pred) q var_scope;
				obj = try_eval (fun q -> q.obj) q var_scope;
				ctxt = q.ctxt; (*FIXME ignored for the time being*)
				time_stp = q.time_stp; (*FIXME ignored for the time being*)
				sign = q.sign (*FIXME ignored for the time being*) } in
		List.fold_right (fun v acc ->
						let (m1, v1) = sub_filter (fun q -> q.subj) q' v in
						let (m2, v2) = sub_filter (fun q -> q.pred) q' v in
						let (m3, v3) = sub_filter (fun q -> q.obj) q' v in
						let scope_ext =
							List.fold_right (fun v acc ->
											match v with
											| None -> acc
											| Some x -> x :: acc) [v1; v2; v3] [] in
						(* FIXME remember that we're ignoring ctxt, time_stp and sign *)
						if m1 && m2 && m3 then
							(scope_ext @ var_scope, v :: pre_soln) :: acc
						else acc) db [] in
	List.fold_right (fun q acc ->
					List.map (fun ps -> make_query' ps q) acc
					|> List.concat) qry [([], [])]
	|> List.map snd;; (*Map ext_tuple to tuple, since we don't need the valuation any longer*)