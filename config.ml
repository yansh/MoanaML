open Core.Std

type obj_t = string

type context =string

type signature

    (* represents how user signs tuples *)
type timestamp

type t = string

type 't element_type =
    Variable of string
  | Constant of 't;;

type element =
    Subject of  t element_type
  | Predicate of t element_type
  | Object of obj_t element_type
  | Context of context element_type
  | Timestamp of timestamp element_type
  | Signature of signature element_type
  | Wildcard

type tuple = Tuple of element * element * element * element * element option * element option

type db

let to_string t =
  match t with
  Tuple(Subject (Constant(s)), Predicate(Constant(p)) , Object (Constant(o)), Context (Constant(c)), Some(Signature(sg)) , Some(Timestamp(ts))) ->
    sprintf "%s %s %s" s p o
  |  Tuple(Subject (Constant(s)), Predicate(Constant(p)) , Object (Constant(o)), _ , _ , _) ->
    sprintf "%s %s %s" s p o
  | _ -> "Not printing this tuple." ;;

let rec print_tuples tuples =
       match tuples with
       | [] -> print_endline "Finished List"
       |  head::rest -> print_endline (to_string head); print_tuples rest;;

let compare t1 t2 =
  let Tuple(s1, p1 , o1, c1, sg1, ts1) = t1 and
    Tuple(s2, p2, o2, c2, sg2, ts2) = t2 in
  if s1=s2 || s1 = Wildcard ||  s2 =  Wildcard then
        begin
          if p1=p2 || p1= Wildcard || p2 = Wildcard then
             if o1=o2 || o1= Wildcard || o2 = Wildcard then
               (print_endline "TRUE!"; true)
             else false
          else false
        end
      else
     (print_endline "Finished FALSE"; false);;

  (*let compare_t t1, t2 = match t1 wth
     | t  * t * obj_t * context * signature option  * timestamp option ->*)
