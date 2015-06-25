{shared{
open Eliom_lib
open Eliom_content
open Html5.D

type timestamp = string deriving (Json)

let nick = "BailaGW"
let channel = "#gb2015"

type message = {
  timestamp : timestamp;
  src     : string;
  dst     : string;
  message : string;
} deriving (Json)
type messages = message list deriving (Json)
}}

module BailaGW_app =
  Eliom_registration.App
    (struct
      let application_name = "BailaGW"
    end)

let main_service =
  Eliom_service.App.service ~path:["BailaGW"; ""] ~get_params:Eliom_parameter.unit ()

open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let messages : messages ref = ref []

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

let bus = Eliom_bus.create ~name:"messages" Json.t<message>

let irc_connection = ref None

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
module S = Sqlexpr

let message_db =
  let db = S.open_db "bailagw.sqlite3" in
  Lwt.async (
    fun () ->
      S.execute db
        sqlinit"CREATE TABLE IF NOT EXISTS message(
              message INTEGER PRIMARY KEY,
              timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
              src TEXT NOT NULL,
              dst TEXT NOT NULL,
              str TEXT NOT NULL
            );"  );
  db

{client{
   let add_message (message : message) =
     (* let area = Eliom_content.Html5.To_dom.of_p %message_area_elt in *)
     (* let () = area##innerHTML##appendData (Js.string message) in *)
     (* let () = area##innerHTML <- area##innerHTML##concat ((Js.string message)##concat (Js.string " moi ")) in *)
     let element =
       div ~a:[a_class ["message"]]
         [span ~a:[a_class ["src"]] [pcdata message.src];
          span ~a:[a_class ["timestamp"]] [pcdata message.timestamp];
          span ~a:[a_class ["text"]] [pcdata message.message]] in
     Eliom_content.Html5.Manip.appendChild %message_area_elt element;
     Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
     ()
}}

let db_add_message message =
  S.execute message_db sql"INSERT INTO message(src, dst, str) VALUES (%s, %s, %s)" message.src message.dst message.message >>= fun () ->
  S.select_one message_db sql"SELECT @s{datetime(timestamp, 'localtime')} FROM message WHERE message = last_insert_rowid()" >>= fun timestamp ->
  Lwt.return { message with timestamp = timestamp }

{server{
  let add_message message =
    messages := !messages @ [message];
    let _ = Eliom_bus.write bus message in
    Lwt.return ()

  let send_add_message = server_function ~name:"send_add_message" Json.t<message> (
    fun (message : message) ->
      ( match !irc_connection with
        | None -> Lwt.return ()
        | Some connection ->
          Lwt.catch (
            fun () ->
              Irc_client_lwt.send_privmsg ~connection ~target:message.dst ~message:(message.src ^ "> " ^ message.message)
          )
            (function exn ->
              Printf.eprintf "Problem writing to socket: %s\n%!" (Printexc.to_string exn);
              Lwt.return ()
            )
      ) >>= fun () ->
      db_add_message message >>= add_message
  )
}}

let () =
  Lwt.async (
    fun () ->
      S.iter message_db
        (fun (timestamp, src, dst, message) ->
           add_message { timestamp; src; dst; message }
        )
        sql"SELECT @s{datetime(timestamp, 'localtime')}, @s{src}, @s{dst}, @s{str} FROM message ORDER BY timestamp"
  )

let backlog_service =
  Eliom_registration.Ocaml.register_service
    ~path:["BailaGW"; "backlog"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (!messages))

{client{
   let start_backlog nick =
     let input_area = Eliom_content.Html5.To_dom.of_textarea %input_area_elt in
     input_area##style##display <- Js.string "block";
     input_area##onkeydown <- Dom_html.handler (
         fun ev ->
           if ev##keyCode = 13 then
             ( let value = Js.to_string input_area##value in
               Lwt.async (fun () -> %send_add_message { timestamp = "now"; src = "BailaGW/" ^ nick; dst = channel; message = value });
               input_area##value <- Js.string "";
               Js._false )
           else
             Js._true
       );
     Eliom_client.call_ocaml_service ~service:%backlog_service () () >>= fun response ->
     (Eliom_content.Html5.To_dom.of_div %message_area_elt)##style##display <- Js.string "block";
     List.iter add_message response;
     Lwt.async (fun () -> Lwt_stream.iter add_message (Eliom_bus.stream %bus));
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
        
   let init_client () =
     Lwt.async (
       fun () ->
         query_nick start_backlog
     )
}}

let () =
  Lwt.async (
    fun () ->
      let rec loop () =
        let cur_nick = ref nick in
        Printf.eprintf "Conencting..\n%!";
        ( Lwt.catch (
            fun () ->
              Irc_client_lwt.connect_by_name ~server:"jeti" ~port:6667 ~username:"BailaGW" ~mode:0 ~realname:"BailaGW" ~nick:!cur_nick () >>= fun c ->
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
            add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Failed to create connection" }
          | `Connection (Some connection) ->
            (* add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Connected" } >>= fun () -> *)
            irc_connection := Some connection;
            Irc_client_lwt.listen ~connection ~callback:(
              fun connection result ->
                let open Irc_message in
                match result with
                | `Ok { command = Other "376" } ->
                  Irc_client_lwt.send_join ~connection ~channel:channel
                | `Ok { command = Other ("433" | "437") } ->
                  cur_nick := !cur_nick ^ "_";
                  Irc_client_lwt.send_nick ~connection ~nick:!cur_nick
                | `Ok { prefix; command = PRIVMSG (dst, message) } ->
                  let message = { timestamp = "now"; src = CCOpt.get "" prefix; dst; message } in
                  db_add_message message >>= add_message
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
       let _ = {unit{ init_client () }} in
       Lwt.return
         (Eliom_tools.F.html
            ~title:"BailaGW"
            ~css:[["css";"BailaGW.css"]]
            Html5.F.(body [
                login_elt;
                message_area_elt;
                input_area_elt;
              ]
              )
         )
    )
