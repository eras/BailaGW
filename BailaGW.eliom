open Common

{shared{
open Eliom_lib
open Eliom_content
open Html5.D

module UTF8 = UCoreLib.UTF8
module UText = UCoreLib.Text

}}

module BailaGW_app =
  Eliom_registration.App
    (struct
      let application_name = "BailaGW"
    end)

let main_service =
  Eliom_service.App.service ~path:["BailaGW"; ""] ~get_params:Eliom_parameter.unit ()

let backlog_service =
  Eliom_registration.Ocaml.register_service
    ~path:["BailaGW"; "backlog"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Messages.get_all () >>= function messages ->
       Lwt.return (List.map Messages.process_message messages))

let no_image_upload_service =
  Eliom_registration.Html5.register_service
    ~path:["BailaGW"; "image_upload"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Lwt.return (
         (Eliom_tools.F.html
            ~title:"Upload it"
            Html5.F.(body [
              ]
              )
         )
       ))  

let image = ref None

let image_upload_service =
  Eliom_registration.Action.register_post_service
    ~fallback:no_image_upload_service
    ~post_params:(Eliom_parameter.file "image")
    (fun () file ->
       Printf.eprintf "Image uploaded to %s\n%!" file.Ocsigen_extensions.tmp_filename;
       image := Some CCIO.(with_in file.Ocsigen_extensions.tmp_filename read_all);
       Lwt.return (
         (* (Eliom_tools.F.html *)
         (*    ~title:"Uploaded" *)
         (*    Html5.F.(body [ *)
         (*      ] *)
         (*      ) *)
         (* ) *)
       ))

{server{
  let config = Config.config Messages.message_db

  let send_add_message = server_function ~name:"send_add_message" Json.t<Messages.message> (
    fun (message : Messages.message) ->
      ( match !(Irc.irc_connection) with
        | None -> Lwt.return ()
        | Some connection ->
          Lwt.catch (
            fun () ->
              assert (message.Messages.dst = config.Config.c_channel);
              Irc_client_lwt.send_privmsg ~connection ~target:message.Messages.dst ~message:(message.Messages.src ^ "> " ^ message.Messages.text)
          )
            (function exn ->
              Printf.eprintf "Problem writing to socket: %s\n%!" (Printexc.to_string exn);
              Lwt.return ()
            )
      ) >>= fun () ->
      Messages.db_add_message message >>= Messages.message_to_clients
  )
}}

{server{
  let () =
    Irc.init config;
    BailaGW_app.register
      ~service:main_service
      (fun () () ->
         let channel = config.Config.c_channel in
         let _ = {unit{ Client.init_client %image_upload_service %backlog_service %send_add_message %channel }} in
         Lwt.return
           (Eliom_tools.F.html
              ~title:"BailaGW"
              ~css:[["BailaGW"; "css";"BailaGW.css"]]
              Html5.F.(body [
                  Client.login_elt;
                  Client.message_area_elt;
                  Client.input_area_elt;
                ]
                )
           )
      )
}}

