let (@.) f g x = g (f x)

{shared{
open Eliom_lib
open Eliom_content
open Html5.D

type timestamp = string deriving (Json)

module UTF8 = UCoreLib.UTF8
module UText = UCoreLib.Text

type config = {
  c_nick       : string;
  c_username   : string;
  c_realname   : string;
  c_channel    : string;
  c_irc_server : string;
  c_irc_port   : int;
}

type message = {
  timestamp : timestamp;
  src       : string;
  dst       : string;
  text      : string;
} deriving (Json)

type messages = message list deriving (Json)

type range = (int * int) deriving (Json)

type fragment =
  | Url of string
deriving (Json)

type processed_message = {
  pm_meta    : (range * fragment) list;
  pm_message : message;
} deriving (Json)
}}

module BailaGW_app =
  Eliom_registration.App
    (struct
      let application_name = "BailaGW"
    end)

let main_service =
  Eliom_service.App.service ~path:["BailaGW"; ""] ~get_params:Eliom_parameter.unit ()

open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let process_message message =
  let urls, tags, text = Urls.urls_tags_of_string message.text in
  {
    pm_meta    = List.map (fun (range, text) -> (range, Url text)) urls;
    pm_message = { message with text };
  }

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

let bus = Eliom_bus.create ~name:"messages" Json.t<processed_message>

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
            );" >>= fun () ->
      S.execute db
        sqlinit"CREATE TABLE IF NOT EXISTS config(
              key TEXT NOT NULL,
              value TEXT NOT NULL
            );"
  );
  db

{client{
   let message_with_meta processed =
     let text = processed.pm_message.text in
     let rec scan offset meta =
       match meta with
       | [] ->
         [pcdata (String.sub text offset (String.length text - offset))]
       | ((ofs, len), Url url)::rest when ofs = offset ->
         Raw.a ~a:[a_href url; a_target "_new"] [pcdata url]::scan (ofs + len) rest
       | (((ofs, _ofs1), _)::_) as meta ->
         assert (ofs > offset);
         pcdata (String.sub text offset (ofs - offset))::scan ofs meta
     in
     scan 0 processed.pm_meta

   let add_processed_message (processed : processed_message) =
     (* let area = Eliom_content.Html5.To_dom.of_p %message_area_elt in *)
     (* let () = area##innerHTML##appendData (Js.string message) in *)
     (* let () = area##innerHTML <- area##innerHTML##concat ((Js.string message)##concat (Js.string " moi ")) in *)
     let message = processed.pm_message in
     let element =
       div ~a:[a_class ["message"]]
         [span ~a:[a_class ["src"]] [pcdata message.src];
          span ~a:[a_class ["timestamp"]] [pcdata message.timestamp];
          span ~a:[a_class ["text"]] (message_with_meta processed)] in
     Eliom_content.Html5.Manip.appendChild %message_area_elt element;
     Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
     ()
}}

let db_add_message message =
  S.execute message_db sql"INSERT INTO message(src, dst, str) VALUES (%s, %s, %s)" message.src message.dst message.text >>= fun () ->
  S.select_one message_db sql"SELECT @s{datetime(timestamp, 'localtime')} FROM message WHERE message = last_insert_rowid()" >>= fun timestamp ->
  Lwt.return { message with timestamp = timestamp }

let config =
  let select_one_opt key = 
    S.select message_db sql"SELECT @s{value} FROM config WHERE key = %s" key >>= function
      | value::[] -> Lwt.return value
      | _ -> Lwt.fail (Invalid_argument ("Missing configuration key " ^ key))
  in
  Lwt_unix.run (
    select_one_opt "nick"       >>= fun c_nick ->
    select_one_opt "username"   >>= fun c_username ->
    select_one_opt "realname"   >>= fun c_realname ->
    select_one_opt "channel"    >>= fun c_channel ->
    select_one_opt "irc_server" >>= fun c_irc_server ->
    select_one_opt "irc_port"   >>= fun c_irc_port ->
    Lwt.return { c_nick; c_username; c_realname;
                 c_channel;
                 c_irc_server; c_irc_port = int_of_string c_irc_port }
  )

{server{
  let message_to_clients (message : message) =
    let _ = Eliom_bus.write bus (process_message message) in
    Lwt.return ()

  let send_add_message = server_function ~name:"send_add_message" Json.t<message> (
    fun (message : message) ->
      ( match !irc_connection with
        | None -> Lwt.return ()
        | Some connection ->
          Lwt.catch (
            fun () ->
              assert (message.dst = config.c_channel);
              Irc_client_lwt.send_privmsg ~connection ~target:message.dst ~message:(message.src ^ "> " ^ message.text)
          )
            (function exn ->
              Printf.eprintf "Problem writing to socket: %s\n%!" (Printexc.to_string exn);
              Lwt.return ()
            )
      ) >>= fun () ->
      db_add_message message >>= message_to_clients
  )
}}

let all_messages_query = sqlc"SELECT @s{datetime(timestamp, 'localtime')}, @s{src}, @s{dst}, @s{str} FROM message ORDER BY timestamp"

let of_sql_message (timestamp, src, dst, text) = { timestamp; src; dst; text }

let () =
  Lwt.async (
    fun () ->
      S.iter message_db
        (of_sql_message @. message_to_clients)
        all_messages_query
  )

let backlog_service =
  Eliom_registration.Ocaml.register_service
    ~path:["BailaGW"; "backlog"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       S.select message_db all_messages_query >>= function messages ->
       Lwt.return (List.map (of_sql_message @. process_message) messages))

{client{
   let start_backlog channel nick =
     let input_area = Eliom_content.Html5.To_dom.of_textarea %input_area_elt in
     input_area##style##display <- Js.string "block";
     input_area##onkeydown <- Dom_html.handler (
         fun ev ->
           if ev##keyCode = 13 then
             ( let value = Js.to_string input_area##value in
               Lwt.async (fun () -> %send_add_message { timestamp = "now"; src = "BailaGW/" ^ nick; dst = channel; text = value });
               input_area##value <- Js.string "";
               Js._false )
           else
             Js._true
       );
     Eliom_client.call_ocaml_service ~service:%backlog_service () () >>= fun response ->
     (Eliom_content.Html5.To_dom.of_div %message_area_elt)##style##display <- Js.string "block";
     List.iter add_processed_message response;
     Lwt.async (fun () -> Lwt_stream.iter add_processed_message (Eliom_bus.stream %bus));
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

let () =
  Lwt.async (
    fun () ->
      let rec loop () =
        let cur_nick = ref config.c_nick in
        Printf.eprintf "Conencting..\n%!";
        ( Lwt.catch (
            fun () ->
              Irc_client_lwt.connect_by_name
                ~server:config.c_irc_server
                ~port:config.c_irc_port
                ~username:config.c_username
                ~mode:8 (* invisible *)
                ~realname:config.c_realname
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
            message_to_clients { timestamp = "now"; src = "BailaGW"; dst = ""; text = "Failed to create connection" } >>= fun () ->
            Lwt_unix.sleep 10.0
          | `Connection (Some connection) ->
            (* add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Connected" } >>= fun () -> *)
            irc_connection := Some connection;
            Irc_client_lwt.listen ~connection ~callback:(
              fun connection result ->
                let open Irc_message in
                match result with
                | `Ok { command = Other "376" } ->
                  Irc_client_lwt.send_join ~connection ~channel:config.c_channel
                | `Ok { command = Other ("433" | "437") } ->
                  cur_nick := !cur_nick ^ "_";
                  Irc_client_lwt.send_nick ~connection ~nick:!cur_nick
                | `Ok { prefix; command = PRIVMSG (dst, text) } ->
                  let text = { timestamp = "now"; src = CCOpt.get "" prefix; dst; text } in
                  db_add_message text >>= message_to_clients
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
       let _ = {unit{ init_client %config.c_channel }} in
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
