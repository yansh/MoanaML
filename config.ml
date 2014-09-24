open Core.Std

exception Wrong_tuple
exception Wrong_template

type obj_t = string

type context =string

type signature

    (* represents how user signs tuples *)
type timestamp

type t = string

type 't element_type =
    Variable of string
  | Constant of 't
  | Wildcard

(* Mappings between the different sorts of data.
 * In this case, since both are represented as strings,
 * the mapping is the identity function; but the mappings
 * can be made more complex when the types t and obj_t
 * are defined further (if they happen to be different).*)
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
  {subj = Constant(s); 
   pred = Constant(p); 
   obj = Constant(o); 
   ctxt = Constant(c); 
   time_stp = _ ; 
   sign = _}-> sprintf "< %s %s %s >" s p o  
  | _ -> "Not printing this tuple." ;;


    
  
let rec print_tuples tuples =
       match tuples with
       | [] -> print_endline "--"
       |  head::rest -> print_endline (to_string head); print_tuples rest;;

let print_tuples_list tuples =
  List.map ~f:(fun t ->print_tuples t) tuples;;


let compare t1 t2 =
  if t1.subj=t2.subj || t1.subj = Wildcard ||  t2.subj =  Wildcard then
        begin
          if t1.pred=t2.pred || t1.pred= Wildcard || t2.pred = Wildcard then
            if t1.obj=t2.obj || t1.obj= Wildcard || t2.obj = Wildcard then
               (print_endline "TRUE!"; true)
             else false
          else false
        end
      else
     (print_endline "Finished FALSE"; false);;

