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

open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let login_elt, login_input_elt =
  let login_input_elt = input ~input_type:`Text () in
  let login_elt =
    div ~a:[a_id "login"; a_style "display: none"]
      [p [pcdata "millä nimellä kuljet?"]; login_input_elt]
  in
  login_elt, login_input_elt
    
let message_area_elt =
  div ~a:[a_id "message_area"; a_style "display: none"] [pcdata "Baila baila"]

let input_area_elt =
  Eliom_content.Html5.D.Raw.(textarea ~a:[a_id "input_area"; a_maxlength 1000; a_style "display: none"] (pcdata ""))

let irc_connection = ref None

{client{
   let message_with_meta processed =
     let text = processed.Messages.pm_message.Messages.text in
     let rec scan offset meta =
       match meta with
       | [] ->
         [pcdata (String.sub text offset (String.length text - offset))]
       | ((ofs, len), Messages.Url url)::rest when ofs = offset ->
         Raw.a ~a:[a_href url; a_target "_new"] [pcdata url]::scan (ofs + len) rest
       | (((ofs, _ofs1), _)::_) as meta ->
         assert (ofs > offset);
         pcdata (String.sub text offset (ofs - offset))::scan ofs meta
     in
     scan 0 processed.Messages.pm_meta

   let add_processed_message (processed : Messages.processed_message) =
     (* let area = Eliom_content.Html5.To_dom.of_p %message_area_elt in *)
     (* let () = area##innerHTML##appendData (Js.string message) in *)
     (* let () = area##innerHTML <- area##innerHTML##concat ((Js.string message)##concat (Js.string " moi ")) in *)
     let message = processed.Messages.pm_message in
     let element =
       div ~a:[a_class ["message"]]
         [span ~a:[a_class ["src"]] [pcdata message.Messages.src];
          span ~a:[a_class ["timestamp"]] [pcdata message.Messages.timestamp];
          span ~a:[a_class ["text"]] (message_with_meta processed)] in
     Eliom_content.Html5.Manip.appendChild %message_area_elt element;
     Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
     ()
}}

let () =
  Lwt.async (
    fun () ->
      Messages.iter_all Messages.message_to_clients
  )

let backlog_service =
  Eliom_registration.Ocaml.register_service
    ~path:["BailaGW"; "backlog"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Messages.get_all () >>= function messages ->
       Lwt.return (List.map Messages.process_message messages))

{server{
  let config = Config.config Messages.message_db

  let send_add_message = server_function ~name:"send_add_message" Json.t<Messages.message> (
    fun (message : Messages.message) ->
      ( match !irc_connection with
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

{client{
   let start_backlog channel nick =
     let input_area = Eliom_content.Html5.To_dom.of_textarea %input_area_elt in
     input_area##style##display <- Js.string "block";
     input_area##onkeydown <- Dom_html.handler (
         fun ev ->
           if ev##keyCode = 13 then
             ( let value = Js.to_string input_area##value in
               Lwt.async (fun () -> %send_add_message Messages.{ timestamp = "now"; src = "BailaGW/" ^ nick; dst = channel; text = value });
               input_area##value <- Js.string "";
               Js._false )
           else
             Js._true
       );
     Eliom_client.call_ocaml_service ~service:%backlog_service () () >>= fun response ->
     (Eliom_content.Html5.To_dom.of_div %message_area_elt)##style##display <- Js.string "block";
     List.iter add_processed_message response;
     Lwt.async (fun () -> Lwt_stream.iter add_processed_message (Eliom_bus.stream %Messages.bus));
     Lwt.return ()

    let query_nick continue =
      let nick_box = (Eliom_content.Html5.To_dom.of_div %login_elt) in
      let nick_input = (Eliom_content.Html5.To_dom.of_input %login_input_elt) in
      nick_box##style##display <- Js.string "block";
      nick_input##onkeydown <- Dom_html.handler (
          fun ev ->
            if ev##keyCode = 13 then
              ( let value = Js.to_string nick_input##value in
                nick_box##style##display <- Js.string "none";
                Lwt.async (fun () -> continue value);
                Js._false )
            else
              Js._true
        );
      Lwt.return ()
        
   let init_client channel =
     Lwt.async (
       fun () ->
         query_nick (start_backlog channel)
     )
}}

{server{
  let () =
    Lwt.async (
      fun () ->
        let rec loop () =
          let cur_nick = ref config.Config.c_nick in
          Printf.eprintf "Conencting..\n%!";
          ( Lwt.catch (
              fun () ->
                Irc_client_lwt.connect_by_name
                  ~server:config.Config.c_irc_server
                  ~port:config.Config.c_irc_port
                  ~username:config.Config.c_username
                  ~mode:8 (* invisible *)
                  ~realname:config.Config.c_realname
                  ~nick:!cur_nick () >>= fun c ->
                Lwt.return (`Connection c)
              )
                (function
  (* ocsigenserver: main: Uncaught Exception: Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") *)
                  | Unix.Unix_error (_, _, _) ->
                    Printf.eprintf "Failed to connect irc server..\n%!";
                    Lwt.return `Reconnect
                  | exn ->
                    Printf.printf "Problem :( %s\n%!" (Printexc.to_string exn);
                    Lwt.return `Reconnect
                )
            >>= fun response ->
            match response with
            | `Reconnect ->
              Lwt_unix.sleep 10.0
            | `Connection None ->
              Printf.eprintf "Failed to get connection\n%!";
              Messages.message_to_clients Messages.{ timestamp = "now"; src = "BailaGW"; dst = ""; text = "Failed to create connection" } >>= fun () ->
              Lwt_unix.sleep 10.0
            | `Connection (Some connection) ->
              (* add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Connected" } >>= fun () -> *)
              irc_connection := Some connection;
              Irc_client_lwt.listen ~connection ~callback:(
                fun connection result ->
                  let open Irc_message in
                  match result with
                  | `Ok { command = Other "376" } ->
                    Irc_client_lwt.send_join ~connection ~channel:config.Config.c_channel
                  | `Ok { command = Other ("433" | "437") } ->
                    cur_nick := !cur_nick ^ "_";
                    Irc_client_lwt.send_nick ~connection ~nick:!cur_nick
                  | `Ok { prefix; command = PRIVMSG (dst, text) } ->
                    let text = Messages.{ timestamp = "now"; src = CCOpt.get "" prefix; dst; text } in
                    Messages.db_add_message text >>= Messages.message_to_clients
                  | `Ok ({ command = PASS _   } as t)
                  | `Ok ({ command = NICK _   } as t)
                  | `Ok ({ command = USER _   } as t)
                  | `Ok ({ command = OPER _   } as t)
                  | `Ok ({ command = MODE _   } as t)
                  | `Ok ({ command = QUIT _   } as t)
                  | `Ok ({ command = SQUIT _  } as t)
                  | `Ok ({ command = JOIN _   } as t)
                  | `Ok ({ command = JOIN0    } as t)
                  | `Ok ({ command = PART _   } as t)
                  | `Ok ({ command = TOPIC _  } as t)
                  | `Ok ({ command = NAMES _  } as t)
                  | `Ok ({ command = LIST _   } as t)
                  | `Ok ({ command = INVITE _ } as t)
                  | `Ok ({ command = KICK _   } as t)
                  | `Ok ({ command = NOTICE _ } as t)
                  | `Ok ({ command = PING _   } as t)
                  | `Ok ({ command = PONG _   } as t)
                  | `Ok ({ command = Other _  } as t) ->
                    Printf.eprintf "%s\n%!" (to_string t);
                    Lwt.return ()
                  | `Error error ->
                    Printf.eprintf "error: %s\n%!" error;
                    Lwt.return ()
              )
          ) >>= loop
        in
        irc_connection := None;
        loop ()
    )

  let () =
    BailaGW_app.register
      ~service:main_service
      (fun () () ->
         let channel = config.Config.c_channel in
         let _ = {unit{ init_client %channel }} in
         Lwt.return
           (Eliom_tools.F.html
              ~title:"BailaGW"
              ~css:[["BailaGW"; "css";"BailaGW.css"]]
              Html5.F.(body [
                  login_elt;
                  message_area_elt;
                  input_area_elt;
                ]
                )
           )
      )
}}

