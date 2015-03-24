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
                      | true -> print_string ("------->[["^node_id ^ "-AM-"^
                              "-vars-"^var^"-"^value^"]]\n") ; return_unit
                      | false ->
                          print_string ("["^node_id ^ "-AM-"^ "-vars-"^var^"-"
                              ^value^"]"^"\n") ;
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
                    print_string ("["^node_id ^ "-AM-"^ "-tuples-"^i^"]\n");
                    View.update v [ node_id; "AM"; "tuples"; i ]
                      (Rete_node_j.string_of_tuple tuple)
                | true -> print_string ("["^node_id ^ "-AM-"^ "-tuples-"^i^"]");
                    return_unit))
        tuples in
    match mem with
    | `AM am ->
        View.mem v [ node_id; "AM"; "pattern" ] >>=
        (function
          | false ->
              print_string ("["^node_id ^ "-AM-"^ "-pattern-"
                  ^Rete_node_j.string_of_tuple am.ptrn^"]\n");
              View.update v [ node_id; "AM"; "pattern" ]
                (Rete_node_j.string_of_tuple am.ptrn)
          | true -> print_string ("["^node_id ^ "-AM-"^
                  "-pattern-"^Rete_node_j.string_of_tuple am.ptrn^"]");
              return_unit)
        >>= fun () -> store_AM_vars am.vrs
            >>= fun () -> store_AM_tuples am.tpls >>=
                fun () -> return v
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
    in let rec print_lst lst =
      function
      | h:: t -> print_string ("\n-->" ^h); print_lst () t
      | [] -> print_string "<<-"
    in
    View.mem v [key; "AM"; "pattern"] >>= function
    | true ->
        print_string ("\n["^key ^ "]");
        View.read_exn v [key; "AM"; "pattern"] >>=
        fun ptrn -> (* get the pattern tuple string *)
            print_string ("["^key ^ "-AM-pattern"^ ptrn);
            View.list v [key; "AM"; "tuples"] >>=
            fun key_list ->
                List.iter (function |
                    k ->
                        match Irmin.Path.String_list.decons k with
                        | Some step ->
                            let s = snd step in
                            print_lst () k
                        
                        | None -> print_string "Empty"
                  )key_list;
                Lwt_list.fold_left_s
                  ( fun acc k ->
                        View.read_exn v k
                        >>= fun tuple ->
                            print_string tuple;
                            return(
                                json_to_tpl (Rete_node_j.tuple_of_string(tuple)) :: acc
                              )
                  )[] key_list >>= fun tpls ->
                    View.list v [key; "AM"; "vars"] >>=
                    fun paths ->
                        Lwt_list.fold_left_s(fun acc sub_path ->
                                View.list v sub_path >>=
                                function
                                | pth (* collection of subtrees *) ->
                                    let h:: _ = pth in
                                    let [_; _; _; var; _] = h in
                                    let lst_pairs =
                                      List.map (fun [key; am; vars; var; value]->
                                              ((Constant value,
                                                  View.read_exn v [key; am; vars; var; value] >>=
                                                  fun tuple ->
                                                      let tpl =
                                                        json_to_tpl
                                                          ((Rete_node_j.tuple_of_string tuple)) in
                                                      return tpl) )
                                        ) pth in
                                    return ((var, lst_pairs):: acc))
                          
                          [] paths
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
                            return { pattern =
                                  json_to_tpl(Rete_node_j.tuple_of_string ptrn);
                                tuples = tpls; vars = vs } >>=
                            fun am ->
                                View.list v [key; "BM"; "sols"] >>=
                                fun paths -> (*--this is done to retrieve var & value --*)
                                    let path:: _ = paths in
                                    View.list v path >>=
                                    fun sub_path ->
                                        let h::_ = sub_path in
                                        let [key; bm; sols; var; value] = h in
                                        (*----------------*)
                                        Lwt_list.map_s (
                                            fun path ->
                                                let get_tuples path =
                                                  print_string "\n+++";
                                                  print_lst () path;
                                                  View.list v path >>=
                                                  fun sub_path ->
                                                      Lwt_list.map_s (fun [key; bm; sols; var; value; idx]->
                                                              print_string ("---->"^idx);
                                                              View.read_exn v [key; bm; sols; var; value; idx] >>=
                                                              fun tuple ->
                                                                  print_string tuple;
                                                                  return(json_to_tpl (Rete_node_j.tuple_of_string tuple)))
                                                        sub_path
                                                in
                                                View.list v path >>=
                                                fun sub_paths ->
                                                    let rec tvrs_ptns paths =
                                                      match paths with
                                                      | sub_path:: t (*[sub_path]*) -> (tvrs_ptns t) @ (Lwt_unix.run(get_tuples sub_path))
                                                      | [] -> []
                                                    in let tuples = tvrs_ptns sub_paths
                                                    in return (var, (Constant value, tuples))) paths
                                        
                                        >>= fun sols ->
                                        (* remove Lwt dependency in sols *)
                                            let sols_ =
                                              List.map
                                                (fun (var, values) ->
                                                      match values with
                                                      | (value, tpls) ->
                                                      
                                                          (var, (value, tpls)))
                                                sols in
                                            return { solutions = sols } >>= fun bm -> return (Node (am, bm,
                                                      Lwt_unix.run(get_node v (node_id +1))))
    | false -> print_string key; return Empty
  
  let rec store_node v idx jnode =
    let open Rete_node_t
    in
    let key = string_of_int idx
    in
    match jnode with
    | `Node (jam , jbm, next_node) ->
        store_mem v ("Node" ^ key) jam >>=
        fun v ->
            store_mem v ("Node" ^ key) jbm >>=
            fun v ->
                store_node v (idx + 1) next_node
    | `BNode tpls -> return v
    
    | `Empty -> return v
  
  let rec store_node v idx jnode =
    let open Rete_node_t
    in
    let key = string_of_int idx
    in
    match jnode with
    | `Node (jam , jbm, next_node) ->
        store_mem v ("Node" ^ key) jam >>=
        fun v ->
            store_mem v ("Node" ^ key) jbm >>=
            fun v ->
                store_node v (idx + 1) next_node
    | `BNode tpls -> return v
    
    | `Empty -> return v
  
  let view_of_t node =
    (Log.debug "view_of_t";
      let jnode = ReteImpl.InMemory.node_to_json node
      in
      View.empty () >>=
      fun v -> store_node v 0 jnode >>= fun v -> return v)
  
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
                View.update_path (t "Rete init") [ "Rete" ] view
                >>=
                fun () ->
                    return (t "??"))
  
  let get t =
    Lwt_unix.run
      (View.of_path t [ "Rete" ] >>= fun v ->
            ReteView.t_of_view v >>= function
            | node -> node)
  
end
