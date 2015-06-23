{shared{
open Eliom_lib
open Eliom_content
open Html5.D

type message = string deriving (Json)
type messages = message list deriving (Json)
}}

module BailaGW_app =
  Eliom_registration.App
    (struct
      let application_name = "BailaGW"
    end)

let main_service =
  Eliom_service.App.service ~path:["BailaGW"] ~get_params:Eliom_parameter.unit ()

open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let messages : messages ref = ref ["initial message"]

let message_area_elt = p [pcdata "Baila baila"]

let input_area_elt = let open Eliom_content.Html5.D in input ~input_type:`Text ()

let bus = Eliom_bus.create Json.t<message>

let irc_connection = ref None

{client{
   let add_message message =
     (* let area = Eliom_content.Html5.To_dom.of_p %message_area_elt in *)
     (* let () = area##innerHTML##appendData (Js.string message) in *)
     (* let () = area##innerHTML <- area##innerHTML##concat ((Js.string message)##concat (Js.string " moi ")) in *)
     let element = p [pcdata message] in
     Eliom_content.Html5.Manip.appendChild %message_area_elt element;
     Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
     ()
}}

{server{
  let add_message message =
    messages := !messages @ [message];
    let _ = Eliom_bus.write bus message in
    Lwt.return ()

  let send_add_message = server_function Json.t<message> (
    fun message ->
      ( match !irc_connection with
        | None -> Lwt.return ()
        | Some connection -> Irc_client_lwt.Client.send_privmsg ~connection ~target:"#gb2015" ~message) >>= fun () ->
      add_message message
  )
}}

let backlog_service =
  Eliom_registration.Ocaml.register_service
    ~path:["backlog"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (!messages))

{client{
   let init_client () =
     Lwt.async (
       fun () ->
         let e = Eliom_content.Html5.To_dom.of_input %input_area_elt in
         e##onkeydown <- Dom_html.handler (
             fun ev ->
               if ev##keyCode = 13 then
                 ( let value = Js.to_string e##value in
                   Lwt.async (fun () -> %send_add_message value);
                   e##value <- Js.string "";
                   Js._false )
               else
                 Js._true
           );
         Eliom_client.call_ocaml_service ~service:%backlog_service () () >>= fun response ->
         List.iter add_message response;
         Lwt.async (fun () -> Lwt_stream.iter add_message (Eliom_bus.stream %bus));
         Lwt.return ()
     )
}}

let () =
  Lwt.async (
    fun () ->
      Lwt.bind (Irc_client_lwt.Client.connect_by_name ~server:"jeti" ~port:6667 ~username:"flux" ~mode:0 ~realname:"flux" ~nick:"flux" ()) @@ fun response ->
      match response with
      | None ->
        add_message "Failed to create connection"
      | Some connection ->
        add_message "Connected" >>= fun () ->
        irc_connection := Some connection;
        Irc_client_lwt.Client.listen ~connection ~callback:(
          fun ~connection ~result ->
            ( match result with
              | Irc_message.Message { Irc_message.command = "376" } ->
                Irc_client_lwt.Client.send_join ~connection ~channel:"#gb2015"
              | Irc_message.Message { Irc_message.prefix; command; params; trail } ->
                Printf.ksprintf add_message "(%s) (%s) (%s)" command (String.concat "," params) (match trail with None -> "-" | Some trail -> trail)
              | Irc_message.Parse_error (_data, message) ->
                Printf.ksprintf add_message "Error in response: %s" message
            )
        )
  )

let () =
  BailaGW_app.register
    ~service:main_service
    (fun () () ->
       let _ = {unit{ init_client () }} in
       Lwt.return
         (Eliom_tools.F.html
            ~title:"BailaGW"
            ~css:[["css";"BailaGW.css"]]
            Html5.F.(body [
                message_area_elt;
                input_area_elt;
              ]
              )
         )
    )
