(*
* Copyright (c) 2014 Yan Shvartzshnaider
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)


(*** This is an implementation of a sample application - contacts

The main functionality:

* Add contact info
* Query info on the contact
***)
  
(* Boilerplate code for calling OCaml in the worker thread. *)
open Contacts

let js_object = Js.Unsafe.variable "Object"
let js_handler = jsnew js_object ()
let postMessage = Js.Unsafe.variable "postMessage"

let log s = ignore (Js.Unsafe.call postMessage (Js.Unsafe.variable "self")
              [|Js.Unsafe.inject (Js.string s)|])

let onmessage event =
  let fname = event##data##fname in
  let args = event##data##args in
  let handle = Js.Unsafe.get js_handler fname in
  let result = Js.Unsafe.fun_call handle (Js.to_array args) in
  let response = jsnew js_object () in
  Js.Unsafe.set response (Js.string "fname") fname;
  Js.Unsafe.set response (Js.string "result") result;
  Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject response|]

let _ = Js.Unsafe.set (Js.Unsafe.variable "self") (Js.string "onmessage") onmessage

let contact_to_json contacts =
  let q2 =
    "MAP {
        ?a,fn,?fn,contacts
    ?a,last,?last,contacts    
    ?a,email,?email,contacts
    ?a,twitter,?tw,contacts
    ?a,mobile,?mobile,contacts
    ?a,title,?title,contacts 
    ?a,image,?image,contacts
    ?a,knows,?friend,contacts 
        }" in
  let v_to_string = function | Config.Constant x -> x | _ -> "" in
  let get_field f map =
    try List.hd (List.map v_to_string (Helper.StringMap.find f map))
    with | Not_found -> "" in
  let get_field_list f map =
    try List.map v_to_string (Helper.StringMap.find f map)
    with | Not_found -> [] in
  let to_json id c =
    let r = Rete.exec_qry q2 c in
    let c_map =
      Rete.get_res_map r
        [ "?fn"; "?last"; "?email"; "?tw"; "?mobile"; "?title"; "?image";
          "?friend"; "?work" ]
    in
      let open Contact_t
      in
        ({
           id = id;
           fn = get_field "?fn" c_map;
           last = get_field "?last" c_map;
           email = get_field "?email" c_map;
           title = Some (get_field "?title" c_map);
           image = Some (get_field "?image" c_map);
           twitterId = Some (get_field "?tw" c_map);
           work = Some (get_field "?work" c_map);
           knows = Some (get_field_list "?friend" c_map);
         } |> Contact_j.string_of_contact) |> Yojson.Basic.from_string
  in List.mapi to_json contacts

(* return to page *) 
let js_query txt  =
		let contacts = Helper.to_tuple_collection (Js.to_string txt) in
    let json = `List(contact_to_json contacts)in let pp =Yojson.Basic.pretty_to_string json in Js.string pp  
 	                     
             
let _ = Js.Unsafe.set js_handler (Js.string "runQuery") (Js.wrap_callback js_query)