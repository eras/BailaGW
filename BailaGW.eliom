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

{server{
let config = Config.config Messages.message_db
}}

let names_service =
  Eliom_registration.Ocaml.register_service
    ~path:["BailaGW"; "cmd"; "names"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Irc.names config.Config.c_channel >>= function names ->
       Lwt.return names)

let debug_service =
  Eliom_registration.Html5.register_service
    ~path:["BailaGW"; "debug"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Eliom_state.Ext.iter_sub_states
         ~state:(Eliom_state.Ext.service_group_state "users")
         (fun x ->
            Printf.printf "Diu\n%!";
            Lwt.return ()
         ) >>= fun () ->
       
       Lwt.return (
         (Eliom_tools.F.html
            ~title:"Debug"
            Html5.F.(body [
                pcdata "moi"
              ]
              )
         )
       )
    )

{server{
  let server_add_message kind (message : Types.message) =
    let open Types in
    Irc.with_connection (fun connection ->
        Lwt.catch (
          fun () ->
            assert (message.dst = config.Config.c_channel);
            (match kind with
             | `Notice -> Irc_client_lwt.send_notice
             | `Message -> Irc_client_lwt.send_privmsg)
              ~connection
              ~target:message.dst
              ~message:(message.src ^ "> " ^
                        match message.contents with
                        | Text text -> text
                        | Image id ->
                          let uri = Eliom_uri.make_string_uri ~absolute:true ~service:ImageDownload.service (id, 0) in
                          Printf.sprintf "Image uploaded: %s" uri
                       )
        )
          (function exn ->
            Printf.eprintf "Problem writing to socket: %s\n%!" (Printexc.to_string exn);
            Lwt.return ()
          )
      ) >>= fun () ->
    Messages.db_add_message message >>= Messages.message_to_clients

  let send_add_message = server_function ~name:"send_add_message" Json.t<Types.message> (server_add_message `Message)
}}

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

let generate_uuid = Uuidm.v4_gen @@ Random.State.make_self_init ()

let image_upload_service =
  Eliom_registration.Html5.register_post_service
    ~fallback:no_image_upload_service
    ~post_params:Eliom_parameter.(file "image" ** string "src" ** string "dst")
    (fun () (file, (src, dst)) ->
       assert (dst = config.Config.c_channel);
       let id = Uuidm.to_string (generate_uuid ()) in
       Printf.eprintf "Image uploaded to %s\n%!" file.Ocsigen_extensions.tmp_filename;
       let dst = Printf.sprintf "images/%s.%d" id 0 in
       Unix.link file.Ocsigen_extensions.tmp_filename dst;
       let args = [|"exiftran"; "-a"; "-i"; dst|] in
       begin Lwt_process.exec ~timeout:10.0 ("/usr/bin/exiftran", args) >>= function
         | Unix.WEXITED 0 -> (* great! *) Lwt.return ()
         | _ -> (* ok, so.. TODO *) Lwt.return ()
       end >>= fun () ->
       let (mime1, mime2) = CCOpt.get ("application", "octetstream") @@ CCOpt.map fst file.Ocsigen_extensions.file_content_type in
       Messages.add_image src dst id (Printf.sprintf "%s/%s" mime1 mime2) 0 >>= fun timestamp ->
       server_add_message `Notice Types.{ src; dst; timestamp; contents = Image id } >>= fun () ->
       Lwt.return (
         (Eliom_tools.F.html
            ~title:"Uploaded"
            Html5.F.(body [
                img ~alt:"Image"
                  ~src:(Eliom_content.Xml.uri_of_fun @@ fun () -> Eliom_uri.make_string_uri ~service:ImageDownload.service (id, 0))
                  ()
              ]
              )
         )
       ))

{server{
  let () =
    Irc.init config;
    BailaGW_app.register
      ~service:main_service
      (fun () () ->
         let foo = Eliom_reference.eref ~scope:Eliom_common.default_session_scope 0 in
         let _ = Eliom_state.set_service_session_group ~scope:Eliom_common.default_session_scope "users" in
         let channel = config.Config.c_channel in
         let client_id = Uuidm.to_string (generate_uuid ()) in
         let client = Clients.add_client client_id in
         let comet_channel = client.Clients.channel in
         let _ = {unit{ Client.init_client { Client.image_upload_service = %image_upload_service;
                                             client_id = %client_id;
                                             comet_channel = %comet_channel;
                                             names_service = %names_service;
                                             backlog_service = %backlog_service;
                                             send_add_message = %send_add_message;
                                             channel = %channel;
                                             nick = (); }
                      }} in
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

