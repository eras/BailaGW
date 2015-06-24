{shared{
open Eliom_lib
open Eliom_content
open Html5.D

type timestamp = string deriving (Json)

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
  Eliom_service.App.service ~path:["BailaGW"] ~get_params:Eliom_parameter.unit ()

open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let messages : messages ref = ref []

let message_area_elt = p [pcdata "Baila baila"]

let input_area_elt = Eliom_content.Html5.D.(input ~input_type:`Text ~a:[a_size 100] ())

let bus = Eliom_bus.create Json.t<message>

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
     let element = p [Printf.ksprintf pcdata "%s %s->%s %s" message.timestamp message.src message.dst message.message] in
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

  let send_add_message = server_function Json.t<message> (
    fun (message : message) ->
      ( match !irc_connection with
        | None -> Lwt.return ()
        | Some connection ->
          Irc_client_lwt.Client.send_privmsg ~connection ~target:message.dst ~message:message.message
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
                   Lwt.async (fun () -> %send_add_message { timestamp = "now"; src = "BailaGW"; dst = "#gb2015"; message = value });
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
        add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Failed to create connection" }
      | Some connection ->
        (* add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Connected" } >>= fun () -> *)
        irc_connection := Some connection;
        Irc_client_lwt.Client.listen ~connection ~callback:(
          fun ~connection ~result ->
            match result with
            | Irc_message.Message { Irc_message.command = "376" } ->
              Irc_client_lwt.Client.send_join ~connection ~channel:"#gb2015"
            | Irc_message.Message { Irc_message.prefix = Some prefix; command = "PRIVMSG"; params = channel::_; trail = Some trail } ->
              let message = { timestamp = "now"; src = prefix; dst = channel; message = trail } in
              db_add_message message >>= add_message
            | Irc_message.Message { Irc_message.prefix; command; params; trail } ->
              (* Printf.ksprintf add_message "(%s) (%s) (%s) (%s)" (match prefix with None -> "-" | Some x -> x) command (String.concat "," params) (match trail with None -> "-" | Some trail -> trail) *)
              Lwt.return ()
            | Irc_message.Parse_error (_data, message) ->
              add_message { timestamp = "now"; src = "BailaGW"; dst = ""; message = "Error in response: " ^ message }
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
