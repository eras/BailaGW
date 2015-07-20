open Common

let irc_connection = ref None

{server{
  type response =
    | R_PRIVMSG
    | R_PASS
    | R_NICK
    | R_USER
    | R_OPER
    | R_MODE
    | R_QUIT
    | R_SQUIT
    | R_JOIN
    | R_JOIN0
    | R_PART
    | R_TOPIC
    | R_NAMES
    | R_LIST
    | R_INVITE
    | R_KICK
    | R_NOTICE
    | R_PING
    | R_PONG
    | R_Other of string

  type callback = {
    id       : int;
    response : response;
    callback : Irc_client_lwt.connection_t -> Irc_message.t -> unit Lwt.t;
  }

  let response_of_irc_message { Irc_message.command = cmd } =
    let open Irc_message in
    match cmd with
    | PRIVMSG _ -> R_PRIVMSG
    | PASS _    -> R_PASS
    | NICK _    -> R_NICK
    | USER _    -> R_USER
    | OPER _    -> R_OPER
    | MODE _    -> R_MODE
    | QUIT _    -> R_QUIT
    | SQUIT _   -> R_SQUIT
    | JOIN _    -> R_JOIN
    | JOIN0     -> R_JOIN0
    | PART _    -> R_PART
    | TOPIC _   -> R_TOPIC
    | NAMES _   -> R_NAMES
    | LIST _    -> R_LIST
    | INVITE _  -> R_INVITE
    | KICK _    -> R_KICK
    | NOTICE _  -> R_NOTICE
    | PING _    -> R_PING
    | PONG _    -> R_PONG
    | Other x   -> R_Other x

  let is_response response callback = callback.response = response

  let callback_id = ref 0
  let callbacks = ref []

  let add_callback response callback =
    incr callback_id;
    callbacks := { response; callback; id = !callback_id }::!callbacks

  let init config =
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
              Messages.message_to_clients Types.{ timestamp = "now"; src = "BailaGW"; dst = ""; contents = Text "Failed to create connection" } >>= fun () ->
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
                    let text = Types.{ timestamp = "now"; src = CCOpt.get "" prefix; dst; contents = Text text } in
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
                    let relevant_callbacks = List.filter (is_response (response_of_irc_message t)) !callbacks in
                    let relevant_ids = List.map (fun callback -> callback.id) relevant_callbacks in
                    callbacks := List.filter (fun callback -> not (List.mem callback.id relevant_ids)) !callbacks;
                    Lwt_list.iter_s (
                      fun callback ->
                        callback.callback connection t
                    ) relevant_callbacks
                  | `Error error ->
                    Printf.eprintf "error: %s\n%!" error;
                    Lwt.return ()
              )
          ) >>= loop
        in
        irc_connection := None;
        loop ()
    )

  let with_connection f =
    match !(irc_connection) with
    | None -> Lwt.return ()
    | Some connection -> f connection

  let names channel =
    let (thread, wake) = Lwt.wait () in
    with_connection (fun connection ->
        let open Irc_message in
        let response = ref [] in
        let rec retrieve_list () =
          add_callback (R_Other "353") @@ fun _connection message ->
          response := message.params::!response;
          retrieve_list ();
          Lwt.return ()
        in
        retrieve_list ();
        add_callback (R_Other "366") (fun _connection message -> Lwt.wakeup wake (List.concat !response) |> Lwt.return);
        Irc_client_lwt.send ~connection {
            prefix  = None;
            command = NAMES [channel];
            params  = [];
        };
      ) >>= fun () ->
    thread
}}
