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


let results = Rete.exec_qry q1 (policies :: contacts) |> Rete.exec_bm q2    
let Rete.Node (_, res_bm, _) = results  
let res_str = List.fold_right (fun tpl acc -> (Helper.to_string tpl)^"\n"^acc)  (Helper.TupleSet.elements (Rete.get_tuples results)) "" 
let js_query query = Js.string(res_str)
                     
             
let _ = Js.Unsafe.set js_handler (Js.string "runQuery") (Js.wrap_callback js_query)