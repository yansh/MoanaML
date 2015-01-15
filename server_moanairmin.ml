(**
 @Program: server_moanairmin.ml
 @author: Carlos Molina-Jimenez
 @date: 5 Jan 2015, Computer Laboratory, Univ. of Cambridge

 @sources: 
  1) I copied the server from
     https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/hello_world.ml
  2) I copied the moana files from 
     https://github.com/yansh/MoanaML/blob/master/moana_irmin.ml
  3) I copied irmin from
     https://github.com/mirage/irmin
            
 @compilation: build.sh

 @Execution: % ser.byte
             Listening for HTTP on port 8080
             Try: 'curl http://localhost:8080/printcontacts?repositoryname=noname'
             
   % curl http://localhost:8080/printcontacts?repositoryname=noname
          < a fn Jon >
          < a last Crowcroft > 
           ...
           No more contacts                 

   % curl http://localhost:8080/getwholemap?repositoryname=abc  

   % curl "http://localhost:8080/query?subject=d&predicate=email" 

   % curl "http://localhost:8080/query?subject=a&predicate=email"

*)


open Core.Std
open Async.Std 
open Cohttp_async

(*
 To integrate later...
 open Createrepository
 open Inputcontact
 open Readcontact
 *)

(*
 * 1) This servers relies on the Contacts module.
 * 2) This version implements only 'testcontacts' 
 *    and 'printcontacts'.
 *)


let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in match Uri.path uri with

  (*| "/createrepository" -> Uri.get_query_param uri "repositoryname"
    |> Option.map ~f:(fun reponame -> Createrepository.createEmptyRepo reponame)
    |> fun someOrNone -> let resp= match someOrNone with
       | None   -> "No param repositoryname supplied\n"
       | Some s -> "The response from irmin is " ^ s ^ " \n" 
       in resp
    |> Server.respond_with_string
   *)

  (*| "/writecontact" -> Uri.get_query_param uri "contactname" 
    |> fun someOrNone -> let contactNameVal= match someOrNone with
       | None      -> raise (Failure "contact name is empty") 
       | Some name -> name 
       in contactNameVal
    |> fun contactNameVal -> let phoneNumVal= match Uri.get_query_param uri "phone" with 
       | None         -> raise (Failure "phone number is empty") 
       | Some number  -> number 
       in contactNameVal^ "/"^phoneNumVal
    |> fun namePhoneNum -> print_string("\n param= " ^ namePhoneNum); Some namePhoneNum
    |> Option.map ~f:(fun namePhoneNum -> Inputcontact.inputcontact namePhoneNum) 
    |> Option.value ~default:"No contact parammeters supplied"
    |> Server.respond_with_string 
   *)


  | "/testcontacts" -> Uri.get_query_param uri "str"
    |> Option.map ~f:(fun str-> Contacts.duplicatestr str)
    |> Option.value ~default:"No param str name supplied"
    |> Server.respond_with_string 

  | "/printcontacts" -> Uri.get_query_param uri "repositoryname"
    |> Option.map ~f:(fun s-> Contacts.getStrOfTuples())
    |> Option.value ~default:"No param repositoryname supplied"
    |> Server.respond_with_string 

  | "/getwholemap" -> Uri.get_query_param uri "repositoryname"
    |> Option.map ~f:(fun s-> Contacts.getWhole_r_map())
    |> Option.value ~default:"No param repositoryname supplied"
    |> Server.respond_with_string 


 | "/query" -> Uri.get_query_param uri "subject"
    |> fun someSubj -> let sbj= match someSubj with
       | None       -> raise (Failure "subject name is empty")
       | Some subj  -> subj
       in sbj 
    |> fun sbj -> let prd= match Uri.get_query_param uri "predicate" with
       | None      -> raise (Failure "predicate is empty")
       | Some pred -> pred
       (* in Some (sbj ^ "/" prd) *)
       in Some (sbj,prd) 
 
    |> Option.map ~f:(fun sbjprd-> Contacts.query_r_map_defPrm 
        ?plcy:(None) ~subj:(fst(sbjprd)) ~pred:(snd(sbjprd)) ?qvar:(None) ?cntx:(None))

    |> Option.value ~default:"No sub or pred parammeters supplied"
    |> Server.respond_with_string


  | _ ->
    Server.respond_with_string ~code:`Not_found "Route not found"

let start_server port () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try: 'curl http://localhost:%d/printcontacts?repositoryname=noname'\n%!" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler
  >>= fun _ -> Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start an Async server that reponds to irmin request filetered by Moana"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
    ) start_server

  |> Command.run

