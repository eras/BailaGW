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

{server{
  let server_add_message kind (message : Messages.message) =
    let open Messages in
    Irc.with_connection (fun connection ->
        Lwt.catch (
          fun () ->
            assert (message.dst = config.Config.c_channel);
            (match kind with
             | `Notice -> Irc_client_lwt.send_notice
             | `Message -> Irc_client_lwt.send_privmsg)
              ~connection
              ~target:message.dst
              ~message:(match message.contents with
                        | Join -> message.src ^ " liittyi kanavalle"
                        | Text text -> message.src ^ "> " ^ text
                        | Image id ->
                          let uri = Eliom_uri.make_string_uri ~absolute:true ~service:ImageDownload.service (id, 0) in
                          message.src ^ "> " ^ Printf.sprintf "Image uploaded: %s" uri
                       )
        )
          (function exn ->
            Printf.eprintf "Problem writing to socket: %s\n%!" (Printexc.to_string exn);
            Lwt.return ()
          )
      ) >>= fun () ->
    db_add_message message >>= message_to_clients

  let send_add_message = server_function ~name:"send_add_message" Json.t<Messages.message> (server_add_message `Message)
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

let image_upload_service =
  let generate_uuid = Uuidm.v4_gen @@ Random.State.make_self_init () in
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
       server_add_message `Notice { Messages.src; dst; timestamp; contents = Messages.Image id } >>= fun () ->
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
         let channel = config.Config.c_channel in
         let _ = {unit{ Client.init_client { Client.image_upload_service = %image_upload_service;
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

