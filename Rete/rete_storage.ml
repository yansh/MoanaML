open Lwt

open Irmin_unix

open ReteImpl.InMemory

open Config
(* Values are Strings * module Store =                                     *)
(* Git.MaKe(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)          *)
module Store = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)

let config = Irmin_git.config ~root: "/tmp/irmin/test/rete" ~bare: false ()

module View = Irmin.View(Store)

module ReteView =
struct
  
  let store_mem v node_id mem =
    let open Rete_node_t
    in
    let store_AM_vars vars =
      Lwt_list.iter_s
        (fun (var, val_pairs) ->
              Lwt_list.iter_s
                (fun (value, tuple) ->
                  (* this check should be redudant, for some reason var,   *)
                  (* value tuples, repeat - NEEDS further investigation    *)
                      View.mem v [ node_id; "AM"; "vars"; var; value] >>=
                      function
                      | true ->
                      (* print_string ("------->[["^node_id ^ "-AM-"^      *)
                      (* "-vars-"^var^"-"^value^"]]\n") ;                  *)
                          return_unit
                      | false ->
                      (* print_string ("["^node_id ^ "-AM-"^               *)
                      (* "-vars-"^var^"-" ^value^"]"^"\n") ;               *)
                          View.update v [ node_id; "AM"; "vars"; var; value ]
                            (Rete_node_j.string_of_tuple tuple)
                ) val_pairs) vars in
    let store_AM_tuples tuples =
      Lwt_list.iteri_s
        (fun i tuple ->
              let i = string_of_int i
              in
              View.mem v [ node_id; "AM"; "tuples"; i ] >>=
              (function
                | false ->
                (* print_string ("["^node_id ^ "-AM-"^                     *)
                (* "-tuples-"^i^"]\n");                                    *)
                    View.update v [ node_id; "AM"; "tuples"; i ]
                      (Rete_node_j.string_of_tuple tuple)
                | true ->
                (* print_string ("["^node_id ^ "-AM-"^ "-tuples-"^i^"]"); *)
                    return_unit))
        tuples in
    match mem with
    | `AM am ->
        View.mem v [ node_id; "AM"; "pattern" ] >>=
        (function
          | false ->
          (* print_string ("["^node_id ^ "-AM-"^ "-pattern-"               *)
          (* ^Rete_node_j.string_of_tuple am.ptrn^"]\n");                  *)
              View.update v [ node_id; "AM"; "pattern" ]
                (Rete_node_j.string_of_tuple am.ptrn)
          | true ->
          (* print_string ("["^node_id ^ "-AM-"^                           *)
          (* "-pattern-"^Rete_node_j.string_of_tuple am.ptrn^"]");         *)
              return_unit)
        >>= fun () -> store_AM_vars am.vrs
            >>= fun () -> store_AM_tuples am.tpls >>=
                fun () -> return v
    | `BM { sols = solutions } ->
        Lwt_list.iter_s
          (fun (var, (value, tuples)) ->
            (* print_string (Rete_node_j.string_of_bm_json( { sols =       *)
            (* solutions }) |> Yojson.Basic.from_string |>                 *)
            (* Yojson.Basic.pretty_to_string);                             *)
            
                Lwt_list.iteri_p
                  (fun i tuple ->
                        let i = string_of_int i
                        in
                        (* let p = print_string (node_id ^ "BM"^ "sols"^   *)
                        (* var^ value^ "--["^ i ^ "]--\n====> adding       *)
                        (* tuple" ^ (Rete_node_j.string_of_tuple tuple)    *)
                        (* ^"\n") in                                       *)
                        View.update v [ node_id; "BM"; "sols"; var; value; i ]
                          (Rete_node_j.string_of_tuple tuple))
                  tuples)
          solutions
        >>= fun () -> return v
  
  let get_AM v node_id =
    View.mem v [ node_id; "AM"; "pattern" ] >>=
    (function
      | true ->
      (* print_string ("\n["^key ^ "]"); *)
          View.read_exn v [node_id; "AM"; "pattern"] >>=
          fun ptrn -> (* get the pattern tuple string *)
          (* print_string ("["^key ^ "-AM-pattern"^ ptrn); *)
              View.list v [node_id; "AM"; "tuples"] >>=
              fun key_list ->
              (* List.iter (function | k -> match                          *)
              (* Irmin.Path.String_list.decons k with | Some step -> let s *)
              (* = snd step in print_lst () k | None -> print_string       *)
              (* "Empty" )key_list;                                        *)
                  Lwt_list.fold_left_s
                    ( fun acc k ->
                          View.read_exn v k
                          >>= fun tuple ->
                          (* print_string tuple; *)
                              return(
                                  json_to_tpl (Rete_node_j.tuple_of_string(tuple)) :: acc
                                )
                    )[] key_list >>= fun tpls ->
                      View.list v [node_id; "AM"; "vars"] >>=
                      fun paths ->
                          Lwt_list.fold_left_s(fun acc sub_path ->
                                  View.list v sub_path >>=
                                  function
                                  | pth (* collection of subtrees *) ->
                                      let h:: _ = pth in
                                      let [_; _; _; var; _] = h in
                                      let lst_pairs =
                                        List.map (fun [key; am; vars; var; value]->
                                                let tpl = View.read_exn v [key; am; vars; var; value] >>=
                                                  fun tuple ->
                                                      return(json_to_tpl
                                                            ((Rete_node_j.tuple_of_string tuple))) in
                                                (Constant value, Lwt_unix.run(tpl))
                                          ) pth in
                                      return ((var, lst_pairs):: acc))
                            
                            [] paths
                          >>= fun vrs ->
                          (* need to adjust the vars element to not        *)
                          (* include Lwt let vs = List.map (fun (var,      *)
                          (* values) -> (var, (List.map (function |        *)
                          (* (value, tpl) -> (value, Lwt_unix.run tpl))    *)
                          (* values))) vrs in                              *)
                              return { pattern =
                                    json_to_tpl(Rete_node_j.tuple_of_string ptrn);
                                  tuples = tpls; vars = vrs }
      | false -> raise AM_not_found )
  
  let get_BM v node_id =
    View.list v [node_id; "BM"; "sols"] >>=
    fun paths ->
    (* --this is done to retrieve var & value -- *)
        let get_var_value_pair path =
          (* print_string "\n+++"; print_lst () path; *)
          let [key; bm; sols; var; value] = path in
          (var, value)
        in
        (* ---------------- *)
        let get_tuples path =
          (* print_string "\n+++"; print_lst () path; *)
          let [key; bm; sols; var; value] = path in
          (* print_string ("\nvalue: " ^ value); *)
          View.list v path >>=
          fun sub_path ->
              Lwt_list.map_s (fun [key; bm; sols; var; value; idx]->
                  (* print_string ("\n---->["^idx^"]"); *)
                      View.read_exn v [key; bm; sols; var; value; idx] >>=
                      fun tuple ->
                      (* print_string tuple; *)
                          return(json_to_tpl (Rete_node_j.tuple_of_string tuple)))
                sub_path in
        (* should return (var, (val, tuples)) *)
        let rec tvrs_ptns sub_paths =
          match sub_paths with
          | sub_sub_path:: t (*[sub_path]*) ->
              (tvrs_ptns t) @ [fst (get_var_value_pair sub_sub_path),
              ( Constant (snd (get_var_value_pair sub_sub_path)),
                (Lwt_unix.run(get_tuples sub_sub_path)))]
          | [] -> []
        in
        Lwt_list.fold_right_s(fun path acc ->
                View.list v path >>=
                fun sub_paths ->
                    return(tvrs_ptns sub_paths @ acc) )paths []
        
        >>= fun sols ->
        (* List.rev is just match the test -- remove it *)
            return { solutions = (List.rev sols) }
  
  let rec get_node v node_id =
    let key = ("Node" ^ string_of_int node_id)
    in let rec print_lst lst =
      function
      | h:: t -> print_string ("\n-->" ^h); print_lst () t
      | [] -> print_string "<<-"
    in
    View.mem v [key; "AM"; "pattern"] >>= function
    | true ->
        get_AM v key >>=
        fun am -> get_BM v key
            >>=
            fun bm ->
                return (Node (am, bm,
                      Lwt_unix.run(get_node v (node_id +1))))
    | false -> return Empty
  
  (** Takes JNode (Rete Json node) and unpacks it into a list of nodes *)
  let unpack jnode =
    let rec get_nodes jnode acc =
      (match jnode with
        | `Node (jam , jbm, next_node) -> get_nodes next_node (jnode:: acc)
        | `BNode tpls -> acc
        | `Empty -> acc)  in get_nodes jnode []
  
  (** use recusion to store JNODE **)
  let rec r_store_node v idx jnode =
    let open Rete_node_t
    in
    let key = string_of_int idx
    in
    match jnode with
    | `Node (jam , jbm, next_node) ->
        r_store_node v (idx + 1) next_node >>=
        fun v ->
            store_mem v ("Node" ^ key) jam >>=
            fun v ->
                store_mem v ("Node" ^ key) jbm
    
    | `BNode tpls -> return v
    
    | `Empty -> return v
  
  (** nonrecursive way to store -- just store AM and BM **)
  let store_node v idx jnode =
    let open Rete_node_t
    in
    let key = string_of_int idx
    in
    match jnode with
    | `Node (jam , jbm, _) ->
        store_mem v ("Node" ^ key) jam >>=
        fun v ->
            store_mem v ("Node" ^ key) jbm
    
    | `BNode tpls -> return v
    
    | `Empty -> return v
  
  let view_of_t node =
    (Log.debug "view_of_t";
      let jnode = ReteImpl.InMemory.node_to_json node
      in
      View.empty () >>=
      let nodes = unpack jnode in
      fun v ->
      (* r_store_node v 0 jnode *)
          List.iteri (fun i node ->
                  Lwt_unix.run(store_node v i node); ()) (List.rev nodes); return v
          
          >>= fun v -> return v)
  
  let t_of_view v =
    return (get_node v 0)
  
end

module RStorage= struct
  
  (** Initialise Rete network; store initial ones **)
  let init rnode =
    Lwt_unix.run
      (Store.create config task >>=
        fun t ->
            ReteView.view_of_t rnode >>=
            fun view ->
                View.update_path (t "Rete init") [ "Rete" ] view
                >>=
                fun () ->
                    return (t "??"))
  
  (** Retrieve Rete network **)
  let get t =
    Lwt_unix.run
      (View.of_path t [ "Rete" ] >>= fun v ->
            ReteView.t_of_view v >>= function
            | node -> node)
  
  (** Add a tuple to suitable AM in the Rete network *)
 (* Not a very optimal way of doing things. We fetch the Rete node to memoryt *)
  let add t tuple =
    let node = get t in
    let new_node = ReteImpl.InMemory.add node tuple in
    Lwt_unix.run
      (ReteView.view_of_t new_node >>=
        fun view ->
            View.update_path t [ "Rete" ] view
            >>=
            fun () ->
                return t)
    
    
  (* TODO: Fix me*)
  (* This is an attempt to avoid loading node into a memory and modify Irmin directly*)
  (*  while addi a tuple to suitable AM in the Rete network *)
  let add_rec t tuple =
    let p = print_string ("adding tuple" ^ (Helper.to_string tuple)) in
    let rec add_rec v idx tuple found =
      let node_id = string_of_int idx in
      let key = ("Node" ^ node_id) in
      View.mem v [key; "AM"; "pattern"] >>= function
      | true ->
          let am = Lwt_unix.run(ReteView.get_AM v key) in
          if (ReteImpl.InMemory.compare am.pattern tuple) then (* match *)
          let p = print_string ("Found! [" ^ key ^"]\n") in
          let open Rete_node_t in
          let new_am = create_am am.pattern (tuple:: am.tuples) in
          let pp = print_string "---" and ppp = ReteImpl.InMemory.print_mappings new_am and p = print_string "<--\n" in
          let next_key = ("Node" ^ (string_of_int (idx +1))) in
          let bm = Lwt_unix.run(ReteView.get_BM v next_key) in
          let pp = print_string ("\n BM-before:---" ^ next_key) and ppp = ReteImpl.InMemory.print_bm bm and p = print_string "**\n" in
          let new_bm = ReteImpl.InMemory.join new_am bm in
          let pp = print_string "\n BM:---" and ppp = ReteImpl.InMemory.print_bm new_bm and p = print_string "==\n" in
          ReteView.store_mem v key (`AM (ReteImpl.InMemory.am_to_json new_am))
          >>= fun v ->
              ReteView.store_mem v next_key (`BM (ReteImpl.InMemory.bm_to_json new_bm)) >>=
              fun v ->
                  add_rec v (idx +1) tuple true (* true send a message to start the trigger effect *)
          else
          if found then
            (***)
            let open Rete_node_t in
            let p = print_string ("Trigger [" ^ key ^"]\n") in
            let am = Lwt_unix.run(ReteView.get_AM v key) in
            let bm = Lwt_unix.run(ReteView.get_BM v key) in
            let new_bm = ReteImpl.InMemory.join am bm in
            (* let pp= print_string "---" and ppp =                      *)
            (* ReteImpl.InMemory.print_bm bm and p = print_string "<--"  *)
            (* in                                                        *)
            let next_key = ("Node" ^ (string_of_int (idx))) in
            ReteView.store_mem v key (`AM (ReteImpl.InMemory.am_to_json am))
            >>= fun v ->
                ReteView.store_mem v next_key (`BM (ReteImpl.InMemory.bm_to_json new_bm)) >>=
                fun v ->
                    add_rec v (idx +1) tuple true (* true send a message to start the trigger effect *)
          else
            add_rec v (idx +1) tuple false
      | false -> print_string "DONE"; return_unit  in
    Lwt_unix.run(View.of_path t [ "Rete" ] >>=
        fun v ->
            add_rec v 0 tuple false >>=
            fun () ->
                View.merge_path_exn t [ "Rete" ] v
                >>= fun() -> return t )
end
