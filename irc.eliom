open Common

let irc_connection = ref None

{server{
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
}}
