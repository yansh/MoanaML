(* Irmin based storage *)
open Moana
  
open Lwt
  
open Irmin_unix
  
open Config
  
module Git =
  IrminGit.FS(struct let root = Some "/tmp/irmin/test"
                        let bare = true
                           end)
  
(* Values are Strings * *)
module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)
  
module TupleView =
  struct
    type s_tuple = { s : string; p : string; o : string }
    
    type t2 = Config.tuple
    
    type t = t2 list
    
    (* create list of tuples from view *)
    let t_of_view v =
      let aux acc i =
        let i = string_of_int i
        in
          (Store.View.read_exn v [ i ]) >>=
            (fun x -> return ((from_json x) :: acc))
      in
        (Store.View.list v [ [] ]) >>=
          (fun key_list ->
             let keys =
               List.map
                 (function | [ i ] -> int_of_string i | _ -> assert false)
                 key_list
             in Lwt_list.fold_left_s aux [] keys)
      
    (* create view from list of tuples *)
    let view_of_t tuples =
      (Store.View.create ()) >>=
        (fun v ->
           (Lwt_list.iteri_s
              (fun i tuple ->
                 let i = string_of_int i
                 in
                   (*let p=print_endline (Yojson.Basic.to_string (to_json () tpl)) in*)
                   Store.View.update v [ i ]
                     (Yojson.Basic.to_string (to_json () tuple)))
              tuples)
             >>=
             (fun () -> (*print_tuples (Lwt_unix.run (t_of_view v));*)
                return v))
      
  end
  
(* module type S = Irmin.S with type Block.key = IrminKey.SHA1.t and type  *)
(* value = TupleView.t and type branch = IrminTag.String.t                 *)
module S : STORE =
  struct
    open TupleView
      
    type t = Store.t
    
    let name = "IrminStore"
      
    let empty = Lwt_unix.run (Store.create ())
      
    let init tuples =
      Lwt_unix.run
        ((Store.create ()) >>=
           (fun t ->
              (TupleView.view_of_t tuples) >>=
                (fun view ->
                   (Store.View.update_path t [ "Tuples" ] view) >>=
                     (fun () ->
                        (*Store.View.of_path t ["Tuples"] >>=  fun v -> print_tuples (Lwt_unix.run (t_of_view v));*)
                        return t))))
      
    (* add tuple view to the storage FIX ME: needs to add to existing tuples *)
    let add storage tuple =
      Lwt_unix.run
        ((TupleView.view_of_t [ tuple ]) >>=
           (fun view ->
              (Store.View.update_path storage [ "Tuples" ] view) >>=
                (fun _ -> return storage)))
      
    let query (store : t) (q : Config.tuple list) =
      Lwt_unix.run
        ((Store.View.of_path store [ "Tuples" ]) >>=
           (fun v ->
              (TupleView.t_of_view v) >>=
                (fun list -> return (Config.execute_query list q))))
      
    (*	(((Store.list store [ [ "Tuples" ] ]) >>=
					(fun paths ->
								Lwt_list.map_s
									(fun path ->
												(Store.View.of_path store path) >>=
												(fun view -> TupleView.t_of_view view))
									paths))
				>>=
				(fun x ->
							(return (List.flatten x)) >>= 
							(fun list -> return (Config.execute_query list q))))*)
    let to_list t =
      Lwt_unix.run
        ((Store.View.of_path t [ "Tuples" ]) >>=
           (fun v -> TupleView.t_of_view v))
      
  end
  
(* Moana GRAPH with List storage as a backend *)
module G : GRAPH =
  struct
    module IS = S
      
    type t = IS.t
    
    let graph = IS.empty
      
    let add ?(g = graph) (tuple : Config.tuple) =
      let s =
        Printf.sprintf "Adding fact to [ %s <- %s ]" IS.name
          (Config.to_string tuple)
      in (print_endline s; IS.add g tuple)
      
    let map ?(g = graph) (query : Config.tuple list) = IS.query g query
      
    let to_string graph =
      let dbList = IS.to_list graph in
      let rec string_lst dbList =
        match dbList with
        | [] -> "Finished\n"
        | head :: rest ->
            (Config.to_string head) ^ ("\n" ^ (string_lst rest))
      in string_lst dbList
      
  end
  
