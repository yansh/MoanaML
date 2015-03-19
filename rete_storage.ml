open Lwt

open Irmin_unix

open ReteImpl.InMemory

open Config
(* Values are Strings * module Store =                                     *)
(* Git.MaKe(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)          *)
module Store = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)

let config = Irmin_git.config ~root: "/tmp/irmin/test/rete" ~bare: true ()

module View = Irmin.View(Store)

module ReteView =
struct
	
	let store_mem v node_id mem =
		let open Rete_node_t
		in
		match mem with
		| `AM am ->
				View.update v [ node_id; "AM"; "pattern" ]
					(Rete_node_j.string_of_tuple am.ptrn)
				>>= fun () ->
						Lwt_list.iteri_s
							(fun i tuple ->
										let i = string_of_int i
										in
										View.update v [ node_id; "AM"; "tuples"; i ]
											(Rete_node_j.string_of_tuple tuple))
							am.tpls >>= fun () -> Lwt_list.iteri_s
									(fun i (var, val_pairs) ->
												Lwt_list.iter_p
													(fun (value, tuple) ->
																View.update v [ node_id; "AM"; "vars"; var; value ]
																	(Rete_node_j.string_of_tuple tuple)) val_pairs) am.vrs
								>>= fun () -> return v
		| `BM { sols = solutions } ->
				Lwt_list.iteri_s
					(fun i (var, (value, tuples)) ->
								Lwt_list.iter_s
									(fun tuple ->
												let i = string_of_int i
												in
												View.update v [ node_id; "BM"; "sols"; var; value; i ]
													(Rete_node_j.string_of_tuple tuple))
									tuples)
					solutions
				>>= fun () -> return v
	
	let rec get_node v node_id =
		let key = ("Node" ^ string_of_int node_id)
		in
		View.mem v [key; "AM"; "pattern"] >>= function
		| true ->
				View.read_exn v [key; "AM"; "pattern"] >>=
				fun ptrn -> (* get the pattern tuple string *)
						View.list v [key; "AM"; "tuples"] >>=
						fun key_list ->
						(* let keys = List.map (function | [ i ] -> print_string (key  *)
						(* ^ "AM"^ "tuples"^ i);int_of_string i | i -> print_string    *)
						(* (key ^ "AM"^ "tuples"^ i); assert false) key_list in        *)
								List.iter (fun [i]-> print_string (key ^ "AM"^ "tuples"^i) )key_list;
								Lwt_list.fold_left_s
									( fun acc [idx] ->
												View.read_exn v [key; "AM"; "tuples"; idx]
												>>= fun tuple ->
														return(json_to_tpl (Rete_node_j.tuple_of_string(tuple)) :: acc) )[] key_list >>= fun tpls ->
										View.list v [key; "AM"; "vars"] >>=
										fun path ->
												return(List.fold_left(fun acc [var; value] ->
																		(var, [(Constant value,
																				View.read_exn v [key; "AM"; "vars"; var; value] >>=
																				fun tuple ->
																						let tpl = json_to_tpl ((Rete_node_j.tuple_of_string tuple)) in
																						return tpl)]) :: acc)
															
															[] path)
												>>= fun vrs ->
												(* need to adjust the vars element to not include  *)
												(* Lwt                                             *)
														let vs = List.map
																(fun (var, values) ->
																			(var,
																				(List.map
																						(function | (value, tpl) ->
																									(value, Lwt_unix.run tpl))
																						values)))
																vrs in
														return { pattern = json_to_tpl(Rete_node_j.tuple_of_string ptrn); tuples = tpls; vars = vs } >>=
														fun am ->
																View.list v [key; "BM"; "sols"] >>=
																fun path ->
																		Lwt_list.fold_left_s (fun acc [var; value] ->
																						View.list v [key; "BM"; "sols"; var; value] >>=
																						fun key_list ->
																								let tuples = Lwt_list.map_s (
																											fun [idx]->
																													View.read_exn v [key; "BM"; "sols"; var; value; idx] >>=
																													fun tuple ->
																															return(json_to_tpl (Rete_node_j.tuple_of_string tuple)))key_list in
																								return ( (var, (Constant value, tuples)) :: acc)
																			) [] path
																		
																		>>= fun sols ->
																		(* remove Lwt dependency in sols *)
																				let sols_ =
																					List.map
																						(fun (var, values) ->
																									match values with
																									| (value, tpls) ->
																											(var, (value, Lwt_unix.run tpls)))
																						sols in
																				return { solutions = sols_ } >>= fun bm -> return (Node (am, bm, Lwt_unix.run(get_node v (node_id +1))))
		| false -> print_string key; return Empty
	
	let rec store_node v idx jnode =
		let open Rete_node_t
		in
		let key = string_of_int idx
		in
		match jnode with
		| `Node (jam , jbm, next_node) ->
				store_mem v ("Node" ^ key) jam >>=
				(fun v1 ->
							store_mem v ("Node" ^ key) jbm >>=
							(fun v ->
										store_node v (idx + 1) next_node))
		| `BNode tpls -> return v
		
		| `Empty -> return v
	
	let view_of_t node =
		(Log.debug "view_of_t";
			let jnode = ReteImpl.InMemory.node_to_json node
			in
			View.empty () >>=
			fun v -> (store_node v 0 jnode) >>= fun v -> return v)
	
	let t_of_view v =
		return (get_node v 0)
	
end

module RStorage = struct
	
	type t = Store.t
	
	let init rnode =
		Lwt_unix.run
			(Store.create config task >>=
				fun t ->
						ReteView.view_of_t rnode >>=
						fun view ->
								View.update_path (t "init rete") [ "Rete" ] view
								>>=
								fun () ->
										return (t "??"))
	
	let get t =
		Lwt_unix.run
			(View.of_path t [ "Rete" ] >>= fun v ->
						ReteView.t_of_view v >>= function
						| node -> node)
	
end
